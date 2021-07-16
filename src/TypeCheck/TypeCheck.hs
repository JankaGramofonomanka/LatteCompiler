{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}

module TypeCheck.TypeCheck where

import Control.Monad.State hiding ( void )
import Control.Monad.Except hiding ( void )

import qualified Syntax.Syntax as S
import Syntax.SyntaxGADT
import TypeCheck.State
import TypeCheck.Getters
import TypeCheck.PuttersAndDeclarations
import Errors
import Syntax.Debloater ( ToBeDebloated(debloat) )



class ToBeTypeChecked pre post where
  typeCheck :: (MonadState TypeCheckState m, MonadError Error m)
    => pre -> m post





instance ToBeTypeChecked S.Program Program where
  typeCheck (S.Program p defs) = do
    foldl declare (pure ()) defs
    okDefs <- foldl appendTypeChecked (pure []) defs

    return $ Program p okDefs

    where
      
      declare :: (MonadState TypeCheckState m, MonadError Error m)
        => m () -> S.TopDef -> m ()
      declare _ def = case def of
        S.FnDef _ retType id params _ -> declareFunc id retType argTypes
          where
            argTypes = map typeOfParam params
        
        S.ClassDef _ id maybeParent body -> declareClass id maybeParent body


      appendTypeChecked :: (MonadState TypeCheckState m, MonadError Error m)
        => m [TopDef] -> S.TopDef -> m [TopDef]
      appendTypeChecked acc def = do
        l <- acc
        okDef <- typeCheck def
        return $ l ++ [okDef]


instance ToBeTypeChecked S.TopDef TopDef where
  typeCheck (S.FnDef p retType id params (S.Block blPos stmts))
    = case anyType retType of
        AnyT retT -> do
          subVarScope
          foldl declParam (pure ()) params

          subVarScope
          okStmts <- foldl appendTypeChecked (pure []) stmts

          dropVarScope
          dropVarScope

          let okParams = map debloat params
          let okBlock = Block blPos okStmts
          let okDef = FnDef p retT (debloat id) okParams okBlock
          return okDef

    where
      declParam :: (MonadState TypeCheckState m, MonadError Error m)
        => m () -> S.Param -> m ()
      declParam _ (S.Param t id) = declareId t id
        


      appendTypeChecked :: (MonadState TypeCheckState m, MonadError Error m)
        => m [Stmt] -> S.Stmt -> m [Stmt]
      appendTypeChecked acc stmt = do
        l <- acc
        okStmt <- typeCheck' stmt
        return $ l ++ [okStmt]


      typeCheck' :: (MonadState TypeCheckState m, MonadError Error m)
        => S.Stmt -> m Stmt
      typeCheck' stmt = case anyType retType of
        AnyT retT -> case stmt of

          S.Ret retPos expr -> do
            okExpr <- getExpr retT expr
            return $ Ret retPos okExpr
          
          S.VRet retPos -> case retT of
            Void _ -> return $ VRet retPos 
            _ -> throwError $ returnVoidError retPos id retT
            
          
          _ -> typeCheck stmt


          


  typeCheck (S.ClassDef p id maybeParent body) = do
    throwTODO

instance ToBeTypeChecked S.Block Block where
instance ToBeTypeChecked S.Stmt Stmt where
--instance ToBeTypeChecked S.Expr (Expr a) where


{-
data TopDef where
  FnDef :: Pos
        -> Type
        -> Ident
        -> [Param]
        -> Block
        -> TopDef

  ClassDef  :: Pos
            -> Ident
            -> Maybe Ident
            -> ClassBody
            -> TopDef

  deriving (Eq, Ord, Show, Read) 


data Param = Param Type Ident deriving (Eq, Ord, Show, Read)

data Block = Block Pos [Stmt] deriving (Eq, Ord, Show, Read)

data Stmt where
  Empty     :: Pos -> Stmt
  BStmt     :: Pos -> Block -> Stmt
  Decl      :: Pos -> Type -> [Item] -> Stmt
  Ass       :: Pos -> Var -> Expr -> Stmt
  Incr      :: Pos -> Var -> Stmt
  Decr      :: Pos -> Var -> Stmt
  Ret       :: Pos -> Expr -> Stmt
  VRet      :: Pos -> Stmt
  Cond      :: Pos -> Expr -> Stmt -> Stmt
  CondElse  :: Pos -> Expr -> Stmt -> Stmt -> Stmt
  While     :: Pos -> Expr -> Stmt -> Stmt
  SExp      :: Pos -> Expr -> Stmt
  For       :: Pos -> Type -> Ident -> Var -> Stmt -> Stmt

  deriving (Eq, Ord, Show, Read)

data Item where
  NoInit  :: Ident -> Item
  Init    :: Ident -> Expr -> Item

  deriving (Eq, Ord, Show, Read)

data Type where
  Int     :: Pos -> Type
  Str     :: Pos -> Type
  Bool    :: Pos -> Type
  Void    :: Pos -> Type
  Arr     :: Type -> Type
  Custom  :: Ident -> Type

  deriving (Eq, Ord, Show, Read)

data Var where
  Var     :: Pos -> Ident -> Var
  Fun     :: Pos -> Ident -> Var
  Member  :: Pos -> Var -> Ident -> Var
  Elem    :: Pos -> Var -> Expr -> Var
  
  deriving (Eq, Ord, Show, Read)

data Expr where
  EVar      :: Pos -> Var -> Expr
  ELitInt   :: Pos -> SInt -> Expr
  ELitBool  :: Pos -> Bool -> Expr
  EApp      :: Pos -> Var -> [Expr] -> Expr
  EString   :: Pos -> SStr -> Expr
  Neg       :: Pos -> Expr -> Expr
  Not       :: Pos -> Expr -> Expr
  EOp       :: Pos -> BinOp -> Expr -> Expr -> Expr
  ERel      :: Pos -> RelOp -> Expr -> Expr -> Expr
  EBool     :: Pos -> BoolOp -> Expr -> Expr -> Expr
  NewArr    :: Pos -> Type -> Expr -> Expr
  NewObj    :: Pos -> Ident -> Expr
  Cast      :: Pos -> Type -> Expr -> Expr

  deriving (Eq, Ord, Show, Read)


data BinOp = Plus Pos | Minus Pos | Times Pos | Div Pos | Mod Pos
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH Pos | LE Pos | GTH Pos | GE Pos | EQU Pos | NE Pos
  deriving (Eq, Ord, Show, Read)

data BoolOp = And Pos | Or Pos
  deriving (Eq, Ord, Show, Read)



data ClassBody = ClassBody Pos [MemberDecl]
  deriving (Eq, Ord, Show, Read)

data MemberDecl where
  AttrDecl :: Pos -> Type -> Ident -> MemberDecl
  MethodDecl :: TopDef -> MemberDecl
  
  deriving (Eq, Ord, Show, Read)

-- -}





