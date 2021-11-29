module Position.EndPosition where

import Position.Position
import qualified FromBNFC.AbsLatte as BNFC
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS

class HasEnd a where
  endPosition :: a -> Pos


instance HasEnd BNFC.Block where
  endPosition (BNFC.Block _ stmts right) = position right

instance HasEnd BNFC.TopDef where
  endPosition def = case def of
    BNFC.FnDef _ _ _ funcBody -> endPosition funcBody
    BNFC.BaseClassDef _ _ classBody -> endPosition classBody
    BNFC.ChildClassDef _ _ _ _ classBody -> endPosition classBody

instance HasEnd BNFC.ClassBody where      
  endPosition (BNFC.ClassBody _ _ right) = position right

instance HasEnd S.Block where
  endPosition (S.Block _ p _) = p

instance HasEnd S.TopDef where
  endPosition def = case def of
    S.FnDef _ _ _ _ funcBody -> endPosition funcBody
    S.ClassDef _ _ _ classBody -> endPosition classBody

instance HasEnd S.ClassBody where      
  endPosition (S.ClassBody _ p _) = p

instance HasEnd DS.Block where
  endPosition (DS.Block _ p _) = p

instance HasEnd DS.FnDef where
  endPosition (DS.FnDef _ _ _ _ funcBody) = endPosition funcBody

instance HasEnd DS.ClassDef where
    endPosition (DS.ClassDef _ _ _ classBody) = endPosition classBody

instance HasEnd DS.ClassBody where      
  endPosition (DS.ClassBody _ p _) = p


