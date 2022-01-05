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
  , PolyKinds
#-}

module LLVM.Convert where

import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Except


import Data.Singletons.Sigma
import Data.Singletons.Prelude hiding ( Error, SLT )
import Data.Singletons
import qualified Data.Dependent.Map as DM
import qualified Data.Some as Sm
import Data.GADT.Compare

import LLVM.LLVM
import LLVM.State
import LLVM.StateUtils
import LLVM.LLVMGetters
import LLVM.TypeConversion
import LLVM.Cleanup


import qualified Syntax.SyntaxDep as DS
import Position.Position
import Syntax.LangElemClasses
import Errors
import Dependent
import BuiltIns
import qualified Constants as C
import SingChar
import LLVM.StateUtils (mkVTableConst)
import qualified Syntax.SyntaxDep as C


addComment' :: LLVMConverter m => DS.Stmt -> m ()
addComment' stmt = case stmt of
  DS.Empty {} -> pass
  DS.BStmt {} -> pass
  DS.Cond _ cond _ -> addComment $ "if (" ++ prtExpr cond ++ ") { ... }"
  DS.CondElse _ cond _ _ ->
    addComment $ "if (" ++ prtExpr cond ++ ") { ... } else { ... }"

  DS.While _ cond _ -> addComment $ "while (" ++ prtExpr cond ++ ") { ... }"

  DS.For p t i arr stm -> addComment
    $ "for (" ++ prtType t ++ " " ++ name i ++ " : " ++ prtExpr arr
    ++ ") { ... }"

  DS.Forever {} -> addComment "while (true) { ... }"

  _ -> addComment (filter (/= '\n') $ prtStmt stmt)

  where
    pass = return ()

addStmt :: LLVMConverter m => DS.Stmt -> m ()
addStmt stmt = addComment' stmt >> case stmt of
  DS.Empty    p -> return ()
  DS.BStmt    p (DS.Block _ _ stmts) -> do
    
    enter <- getNewLabel "EnterSubScope"
    exit <- getNewLabel "ExitSubScope"
    
    branch' enter
  
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

  DS.For p t i expr stm -> do
    let singT = DS.singFromKW t
    let elemT = sGetPrimType singT
    arr <- getExprValue expr
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
  DS.BStmt _ (DS.Block _ _ stmts) -> mapM_ addStmt stmts

  _ -> addStmt stmt

-------------------------------------------------------------------------------
addFnDef :: LLVMConverter m => DS.FnDef -> m ()
addFnDef = addCallableDef Nothing

addMethodDef :: LLVMConverter m => DS.ClassIdent cls -> DS.FnDef -> m ()
addMethodDef cls = addCallableDef (Just cls)

addCallableDef :: LLVMConverter m
  => Maybe (DS.ClassIdent cls) ->  DS.FnDef -> m ()
addCallableDef mbOwner (DS.FnDef p t funcId params (DS.Block _ _ stmts))
  = do

  --resetCounters
  (primTs, primArgs) <- getParams params
  primTs' :&: primArgs' <- getParams' mbOwner primTs primArgs
  let currentFunc = PotFunc { label       = FuncLabel (name funcId)
                            , retType     = DS.singFromKW t
                            , argTypes    = primTs'
                            , args        = primArgs'
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

  where

    getParams' :: LLVMConverter m
      => Maybe (DS.ClassIdent cls)
      -> SList ts
      -> ParamList ts
      -> m (Sigma [PrimType] (TyCon1 (DList Reg)))
    getParams' mbOwner primTs primArgs = case mbOwner of
      Nothing -> return
          $ primTs :&: primArgs

      Just (DS.ClassIdent _ cls) -> return
          $ SCons (SPtr $ SStruct cls) primTs
        :&: SpecialReg C.selfParam :> primArgs
                          
      
            



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
addClassDef (DS.ClassDef p clsId mbParent (DS.ClassBody _ _ memberDecls)) = do
  addCustomType clsId
  declareParentMembers mbParent
  mapM_ declareAttr memberDecls
  mapM_ convertMethod' memberDecls
  addConstructor clsId
  
  where
    declareParentMembers :: LLVMConverter m => Maybe DS.SomeClassIdent -> m ()
    declareParentMembers Nothing = return ()
    declareParentMembers (Just (_ :&: (DS.ClassIdent _ parent))) = do
      
      ClassInfo
        { vtableInfo = vtable
        , .. } <- getClassInfo parent

      putClassInfo clsId
        $ ClassInfo
          { vtableInfo = mkChildVTable parent vtable
          , constr = Nothing
          , .. }

    declareAttr :: LLVMConverter m => DS.MemberDecl -> m ()
    declareAttr (DS.AttrDecl p t x)
      = addAttr clsId (DS.singFromKW t) x
      
    declareAttr (DS.MethodDecl (DS.FnDef _ t (DS.FuncIdent _ f) ts _))
      = do
        let singT = sGetPrimType $ DS.singFromKW t
        (singTs, _) <- getParams ts
        declareMethod clsId singT singTs (FuncLabel f)


    convertMethod' (DS.MethodDecl def) = addMethodDef clsId def
    convertMethod' (DS.AttrDecl p t x) = return ()

    mkChildVTable :: SStr cls
      -> M.Map String FuncOrParent
      -> M.Map String FuncOrParent
    
    mkChildVTable cls m = M.fromList $ mkList m where
      mkList :: M.Map String FuncOrParent -> [(String, FuncOrParent)]
      mkList m = foldl inheritMethod [] (M.toList m)

      inheritMethod l (methodName, t :&&: x) = case insertParam2' x of
        Left (Some cls1)
          -> l ++ [(methodName, t :&&: extractParam2' (Left $ Some cls1))]

        Right _
          -> l ++ [(methodName, t :&&: extractParam2' (Left $ Some cls))]


addConstructor :: LLVMConverter m => DS.ClassIdent cls -> m ()
addConstructor clsId@(DS.ClassIdent _ cls) = do
  ClassInfo
    { attrs = attrs
    , attrTypes = attrTypes
    , .. } <- getClassInfo cls

  let currentFunc = PotFunc { label       = FuncLabel C.constrLabel
                            , retType     = DS.STVoid
                            , argTypes    = SCons (SPtr $ SStruct cls) SNil
                            , args        = SpecialReg C.selfParam :> DNil
                            , body        = M.empty
                            , blockOrder  = []

                            , owner       = Just clsId
                            }

  putCurrentFunc currentFunc

  l <- getNewLabel C.entryLabel
  newEntryBlock l

  -- store the vtable
  vtablePtr <- getVTablePtr (SStruct cls) self
  
  let vtableVal = ConstPtr $ mkVTableConst cls
  addInstr $ Store (SPtr $ SVTable cls) vtableVal (Var vtablePtr)

  -- initialize attributes
  initAttrs attrTypes attrs
  finishFunc fakePos

  where
    initAttrs :: LLVMConverter m => DList DS.SLatteType ts -> DList DS.Ident ts -> m ()
    initAttrs DNil DNil = return ()
    initAttrs (t :> ts) (attr :> attrs) = initAttr t attr >> initAttrs ts attrs

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
addProg (DS.Program _ defs) = do
  mapM_ addClassDef' defs
  mapM_ addFnDef' defs

  where
    addFnDef' (Left def) = pass
    addFnDef' (Right def) = addFnDef def

    addClassDef' (Left def) = addClassDef def
    addClassDef' (Right def) = pass

    pass = return ()


extractLLVM :: LLVMConverter m => m LLVMProg
extractLLVM = do
  PotProg { mainFunc = mbMain, funcs = funcs, .. } <- gets currentProg
  case mbMain of
    Nothing -> throwError noMainError
    Just main -> do

      strConstnts <- gets $ M.toList . strLitMap
      clsM <- gets classMap
      customTs <- mapM mkStructDef $ dmToList clsM
      
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
    dmToList :: GCompare (Sing :: t -> *)
      => DM.DMap (Sing :: t -> *) a -> [Sigma t (TyCon1 a)]
    dmToList m = map (getValue m) (DM.keys m)

    getValue  :: GCompare (Sing :: t -> *)
              => DM.DMap (Sing :: t -> *) a
              -> Sm.Some (Sing :: t -> *)
              -> Sigma t (TyCon1 a)
    getValue m (Sm.Some k) = case DM.lookup k m of
      Nothing -> undefined
      Just v -> k :&: v

    mkStructDef :: LLVMConverter m
      => Sigma Str (TyCon1 ClassInfo) -> m (Some StructDef)
    mkStructDef (_ :&: ClassInfo { constr = Nothing , .. })
      = throwError internalNoConstructorError

    mkStructDef (name :&: ClassInfo
                          { attrTypes = ts
                          , vtableInfo = vtableInfo
                          , methods = methods
                          , constr = Just constr
                          , .. })
      = do
        vtable <- mkVTable methods vtableInfo
        return $ 
          Some $ StructDef name (dGetPrimTypes ts) vtable constr

      where
        dGetPrimTypes ::
          DList DS.SLatteType ts -> DList SPrimType (GetPrimTypes ts)
        dGetPrimTypes DNil = DNil
        dGetPrimTypes (x :> xs) = sGetPrimType x :> dGetPrimTypes xs

        mkVTable  :: LLVMConverter m =>
                  [SomeFuncLabel]
                  -> M.Map String FuncOrParent
                  -> m [(String, FuncOrParent)]
        mkVTable [] _ = return $ []
        mkVTable ((_ :&&: FuncLabel f) : funcs) table
          = case M.lookup f table of
              Nothing -> throwError internalMethodInfoMismatchError
              Just x -> do
                funcs' <- mkVTable funcs table
                return $ (f, x) : funcs'
        
    
    

