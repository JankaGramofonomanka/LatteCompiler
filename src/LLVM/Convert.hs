{-# LANGUAGE
    RecordWildCards
  , FlexibleContexts
  
  , DataKinds
  , ScopedTypeVariables
  , KindSignatures
  , TypeApplications
  , TypeOperators
  , TypeFamilies
  , GADTs
  , StandaloneKindSignatures
#-}

module LLVM.Convert where

import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Except


import Data.Singletons.Sigma
import Data.Singletons.Prelude hiding ( Error, SLT )
import Data.Singletons


import LLVM.LLVM
import LLVM.State
import LLVM.StateUtils
import LLVM.LLVMGetters
import LLVM.TypeConversion
import LLVM.Cleanup


import qualified Syntax.SyntaxDep as DS
import Position.Position
import LangElemClasses
import Errors
import Dependent
import BuiltIns
import qualified Constants as C
import SingChar


addStmt :: LLVMConverter m => DS.Stmt -> m ()
addStmt stmt = case stmt of
  DS.Empty    p -> return ()
  DS.BStmt    p (DS.Block _ stmts) -> do
    
    enter <- getNewLabel "EnterSubScope"
    exit <- getNewLabel "ExitSubScope"
    
    branch' enter
    subScope $ do
      newBlock enter
      mapM_ addStmt stmts
      branch' exit

    newBlock exit    

  DS.Decl     p t items -> do
    foldl (\m item -> m >> declareItem t item) (pure ()) items
    
  DS.Ass      p singT var expr -> do
    val <- getExprValue expr
    overwriteVar singT var val

  DS.Incr     p var -> do
    val <- getVarValue DS.STInt var
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation i32 ADD val (ILit 1)
    overwriteVar DS.STInt var (Var reg)

  DS.Decr     p var -> do
    val <- getVarValue DS.STInt var
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation i32 SUB val (ILit 1)
    overwriteVar DS.STInt var (Var reg)

  DS.Ret      p singT expr -> do
    v <- getExprValue expr
    ret' (sGetPrimType singT) v

  DS.VRet     p -> do
    retVoid'

  DS.Cond     p expr stm -> do
    (labelIf, _, labelJoin) <- getIfElseLabels
    cond <- getExprValue expr
    condBranch' cond labelIf labelJoin

    newBlock labelIf
    addStmtIgnoreBlock stm
    branch' labelJoin

    newBlock labelJoin

  DS.CondElse p expr stmIf stmElse -> do
    (labelIf, labelElse,  labelJoin) <- getIfElseLabels
    cond <- getExprValue expr
    condBranch' cond labelIf labelElse

    newBlock labelIf
    addStmtIgnoreBlock stmIf
    branch' labelJoin

    newBlock labelElse
    addStmtIgnoreBlock stmElse
    branch' labelJoin

    newBlock labelJoin

  DS.While p expr stm -> do
    (labelCond, labelLoop, labelJoin) <- getWhileLabels
    
    -- jimp to cond
    branch' labelCond

    -- loop
    newBlock labelLoop
    addStmtIgnoreBlock stm
    branch' labelCond

    -- cond
    newBlock labelCond
    cond <- getExprValue expr
    condBranch' cond labelLoop labelJoin

    newBlock labelJoin

  DS.SExp p singT expr -> do
    _ <- getExprValue expr
    return ()

  DS.For p t i var stm -> subScope $ do
    let singT = DS.singFromKW t
    let elemT = sGetPrimType singT
    arr <- getVarValue (DS.SArr singT) var
    declareItem t (DS.NoInit i)

    iter0 <- getNewReg C.regIter
    iter1 <- getNewReg C.regIter

    -- jump to cond
    labelInit <- getCurrentBlockLabel
    (labelCond, labelLoop, labelJoin) <- getWhileLabels
    branch' labelCond

    -- loop
    newBlock labelLoop
    elem <- getElem elemT arr (Var iter0)
    overwriteVar singT (DS.Var (position i) i) elem

    addStmtIgnoreBlock stm
    addInstr $ Ass iter1 $ BinOperation i32 ADD (Var iter0) (ILit 1)

    labelLoopExit <- getCurrentBlockLabel
    branch' labelCond

    -- cond
    newBlock labelCond
    
    addInstr $ Ass iter0
      $ Phi i32 [(labelInit, ILit 0), (labelLoopExit, Var iter1)]
    len <- getArrLength (SArrStruct elemT) arr
    cond <- getNewReg C.regCond
    addInstr $ Ass cond $ ICMP i32 SLT (Var iter0) len
    condBranch' (Var cond) labelLoop labelJoin

    newBlock labelJoin


  DS.Forever p stm -> do
    (labelCond, labelLoop, labelJoin) <- getWhileLabels
    
    branch' labelLoop

    newBlock labelLoop
    addStmtIgnoreBlock stm
    branch' labelLoop




declareItem :: LLVMConverter m => DS.TypeKW t -> DS.Item t -> m ()
declareItem kw it = case it of
  (DS.NoInit x) -> getDefaultValue kw >>= declIt kw x

  (DS.Init x e) -> getExprValue e >>= declIt kw x

  where
    declIt kw x v = do
      let singT = DS.singFromKW kw
      l <- getCurrentBlockLabel
      assignValue l (typedIdent singT x) v


overwriteVar :: LLVMConverter m
  => Sing t -> DS.Var t -> Value (GetPrimType t) -> m ()
overwriteVar singT var val = case var of
  DS.Var p x -> do
    l <- getCurrentBlockLabel
    assignValue l (typedIdent singT x) val

  DS.Attr p cls e attrId -> do
    let SPtr clsT = sGetPrimType cls
    let attrT = sGetPrimType singT

    eVal <- getExprValue e
    attrPtr <- getAttrPtr attrT clsT eVal attrId
    addInstr $ Store attrT val (Var attrPtr)


  DS.Length p t e -> do
    let SPtr arrT = sGetPrimType t
    eVal <- getExprValue e
    
    lengthPtr <- getArrLengthPtr arrT eVal
    addInstr $ Store i32 val (Var lengthPtr)

  DS.Elem p e i -> do
    let elemT = sGetPrimType singT
    eVal <- getExprValue e
    iVal <- getExprValue i
    
    elemPtr <- getElemPtr elemT eVal iVal
    addInstr $ Store elemT val (Var elemPtr)

  DS.Null   {} -> throwTODOP (position var)
  DS.Self   {} -> throwTODOP (position var)


addStmtIgnoreBlock :: LLVMConverter m => DS.Stmt -> m ()
addStmtIgnoreBlock stmt = case stmt of
  DS.BStmt _ (DS.Block _ stmts) -> subScope $ mapM_ addStmt stmts

  _ -> addStmt stmt

-------------------------------------------------------------------------------
addFnDef :: LLVMConverter m => DS.FnDef -> m ()
addFnDef = addCallableDef Nothing

addMethodDef :: LLVMConverter m => DS.ClassIdent cls -> DS.FnDef -> m ()
addMethodDef cls = addCallableDef (Just cls)

addCallableDef :: LLVMConverter m
  => Maybe (DS.ClassIdent cls) ->  DS.FnDef -> m ()
addCallableDef mbOwner (DS.FnDef p t funcId params (DS.Block _ stmts))
  = subScope $ do

  (primTs, primArgs) <- getParams params
  let currentFunc = PotFunc { label       = FuncLabel (name funcId)
                            , retType     = DS.singFromKW t
                            , argTypes    = primTs
                            , args        = primArgs
                            , body        = M.empty
                            , blockOrder  = []

                            , owner       = mbOwner
                            }

  putCurrentFunc currentFunc

  l <- getNewLabel (name funcId)
  newEntryBlock l
  declareParams l primArgs params

  mapM_ addStmt stmts
  finishFunc p


getParams :: LLVMConverter m
 => DS.ParamList ts -> m (SList (GetPrimTypes ts), ParamList (GetPrimTypes ts))
getParams DNil = return (SNil, DNil)
getParams (DS.Param kw x :> params) = do
  (types, args) <- getParams params
  reg <- getNewReg (name x)
  return (SCons (sGetPrimType $ DS.singFromKW kw) types, reg :> args)

declareParams :: LLVMConverter m
  => Label -> ParamList (GetPrimTypes ts) -> DS.ParamList ts -> m ()
declareParams _ DNil DNil = return ()
declareParams l (reg :> regs) (DS.Param kw x :> params) = do
  let singT = DS.singFromKW kw
  let typedX = typedIdent singT (DS.Scoped 0 x)
  assignValue l typedX (Var reg)
  declareParams l regs params


-------------------------------------------------------------------------------
addClassDef :: LLVMConverter m => DS.ClassDef -> m ()
addClassDef (DS.ClassDef p clsId mbParent (DS.ClassBody _ memberDecls)) = do
  addCustomType clsId
  declareParentMembers mbParent
  mapM_ declareAttr memberDecls
  addConstructor clsId
  
  where
    declareParentMembers :: LLVMConverter m => Maybe DS.SomeClassIdent -> m ()
    declareParentMembers Nothing = return ()
    declareParentMembers (Just (_ :&: parent)) = do
      
      ClassInfo
        { attrs = attrs
        , attrTypes = attrTypes
        , .. } <- getClassInfo parent

      putClassInfo clsId
        $ ClassInfo { attrs = attrs , attrTypes = attrTypes, methods = [] }

    declareAttr :: LLVMConverter m => DS.MemberDecl -> m ()
    declareAttr (DS.AttrDecl p t x)
      -- = addAttr clsId (sGetPrimType $ DS.singFromKW t) x
      = addAttr clsId (DS.singFromKW t) x
      
    declareAttr (DS.MethodDecl def) = addMethodDef clsId def


addConstructor :: LLVMConverter m => DS.ClassIdent cls -> m ()
addConstructor clsId@(DS.ClassIdent _ cls) = subScope $ do
  ClassInfo
    { attrs = attrs
    , attrTypes = attrTypes
    , .. } <- getClassInfo clsId

  let currentFunc = PotFunc { label       = FuncLabel C.constrLabel
                            , retType     = DS.STVoid
                            , argTypes    = SNil
                            , args        = DNil
                            , body        = M.empty
                            , blockOrder  = []

                            , owner       = Just clsId
                            }

  putCurrentFunc currentFunc

  l <- getNewLabel C.entryLabel
  newEntryBlock l
  

  initAttrs attrTypes attrs
  finishFunc fakePos

  where
    initAttrs :: LLVMConverter m => DList DS.SLatteType ts -> DList DS.Ident ts -> m ()
    initAttrs DNil DNil = return ()
    initAttrs (t :> ts) (attr :> attrs) = initAttr t attr

    initAttr :: LLVMConverter m => Sing t -> DS.Ident t -> m ()
    initAttr t attr = do
      val <- getDefaultValue $ DS.kwFromSing p t
      let var = DS.Attr p singCls (DS.EVar p singCls $ DS.Self p) attr

      overwriteVar t var val

      where
        p = position attr
    
    singCls = DS.SCustom cls
      

-------------------------------------------------------------------------------
addProg :: LLVMConverter m => DS.Program -> m ()
addProg (DS.Program _ defs) = mapM_ addFnDef' defs where
  addFnDef' (Left def) = addClassDef def
  addFnDef' (Right def) = addFnDef def


extractLLVM :: LLVMConverter m => m LLVMProg
extractLLVM = do
  PotProg { mainFunc = mbMain, funcs = funcs, .. } <- gets currentProg
  case mbMain of
    Nothing -> throwError noMainError
    Just main -> do

      strConstnts <- gets $ M.toList . strLitMap
      customTs <- gets $ map mkStructDef . M.toList . classMap
      
      mallocTs <- gets mallocTypes
      arrTs <- gets arrTypes

      let prog = LLVM { mainFunc    = main
                      , funcs       = funcs
                      , externFuncs = externFuncLabels
                      , strLits     = strConstnts
                      
                      , customTs  = customTs
                      , mallocTs  = mallocTs
                      , arrTs     = arrTs
                      }

      return prog

  where
    mkStructDef :: (Str, ClassInfo) -> Some StructDef
    mkStructDef (name, ClassInfo { attrTypes = ts, methods = methods, .. })
      = case toSing name of
        SomeSing s -> Some $ StructDef s (dGetPrimTypes ts) methods

      where
        dGetPrimTypes ::
          DList DS.SLatteType ts -> DList SPrimType (GetPrimTypes ts)
        dGetPrimTypes DNil = DNil
        dGetPrimTypes (x :> xs) = sGetPrimType x :> dGetPrimTypes xs

