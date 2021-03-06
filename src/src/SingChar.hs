{-# LANGUAGE
    TemplateHaskell
  , KindSignatures
  , StandaloneKindSignatures
  , DataKinds
  , GADTs
  , TypeFamilies
  , PolyKinds
  , TypeOperators
  , StandaloneDeriving

  , TypeSynonymInstances
  , FlexibleInstances
#-}

module SingChar where

import Control.Monad.Except

import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Kind ( Type )
import Data.GADT.Compare


import Dependent

data Ch
  = C_ | C' | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C0
  | Cq | Cw | Ce | Cr | Ct | Cy | Cu | Ci | Co | Cp | Ca | Cs | Cd | Cf | Cg 
  | Ch | Cj | Ck | Cl | Cz | Cx | Cc | Cv | Cb | Cn | Cm 
  | CQ | CW | CE | CR | CT | CY | CU | CI | CO | CP | CA | CS | CD | CF | CG 
  | CH | CJ | CK | CL | CZ | CX | CC | CV | CB | CN | CM

  deriving (Ord, Eq)

genSingletons [''Ch]

deriving instance Eq (SCh ch)
deriving instance Ord (SCh ch)

fromChar :: Char -> Ch
fromChar ch = case ch of
  { 
    '_' -> C_; '\'' -> C'; 
    '1' -> C1; '2' -> C2; '3' -> C3; '4' -> C4; '5' -> C5; '6' -> C6; 
    '7' -> C7; '8' -> C8; '9' -> C9; '0' -> C0;

    'q' -> Cq; 'w' -> Cw; 'e' -> Ce; 'r' -> Cr; 't' -> Ct; 'y' -> Cy; 
    'u' -> Cu; 'i' -> Ci; 'o' -> Co; 'p' -> Cp; 'a' -> Ca; 's' -> Cs; 
    'd' -> Cd; 'f' -> Cf; 'g' -> Cg; 'h' -> Ch; 'j' -> Cj; 'k' -> Ck; 
    'l' -> Cl; 'z' -> Cz; 'x' -> Cx; 'c' -> Cc; 'v' -> Cv; 'b' -> Cb; 
    'n' -> Cn; 'm' -> Cm; 
    
    'Q' -> CQ; 'W' -> CW; 'E' -> CE; 'R' -> CR; 'T' -> CT; 'Y' -> CY; 
    'U' -> CU; 'I' -> CI; 'O' -> CO; 'P' -> CP; 'A' -> CA; 'S' -> CS; 
    'D' -> CD; 'F' -> CF; 'G' -> CG; 'H' -> CH; 'J' -> CJ; 'K' -> CK; 
    'L' -> CL; 'Z' -> CZ; 'X' -> CX; 'C' -> CC; 'V' -> CV; 'B' -> CB; 
    'N' -> CN; 'M' -> CM; 
    
    c -> error $ "INTERNAL ERROR (unknown symbol '" ++ [c] ++ "')";
  }

someFromChar :: Char -> SomeSing Ch
someFromChar = toSing . fromChar

toChar :: Ch -> Char
toChar ch = case ch of
  {
    C_ -> '_'; C' -> '\'';
    
    C1 -> '1'; C2 -> '2'; C3 -> '3'; C4 -> '4'; C5 -> '5'; C6 -> '6'; 
    C7 -> '7'; C8 -> '8'; C9 -> '9'; C0 -> '0';
    
    Cq -> 'q'; Cw -> 'w'; Ce -> 'e'; Cr -> 'r'; Ct -> 't'; Cy -> 'y';
    Cu -> 'u'; Ci -> 'i'; Co -> 'o'; Cp -> 'p'; Ca -> 'a'; Cs -> 's';
    Cd -> 'd'; Cf -> 'f'; Cg -> 'g'; Ch -> 'h'; Cj -> 'j'; Ck -> 'k';
    Cl -> 'l'; Cz -> 'z'; Cx -> 'x'; Cc -> 'c'; Cv -> 'v'; Cb -> 'b';
    Cn -> 'n'; Cm -> 'm';
    
    CQ -> 'Q'; CW -> 'W'; CE -> 'E'; CR -> 'R'; CT -> 'T'; CY -> 'Y';
    CU -> 'U'; CI -> 'I'; CO -> 'O'; CP -> 'P'; CA -> 'A'; CS -> 'S';
    CD -> 'D'; CF -> 'F'; CG -> 'G'; CH -> 'H'; CJ -> 'J'; CK -> 'K';
    CL -> 'L'; CZ -> 'Z'; CX -> 'X'; CC -> 'C'; CV -> 'V'; CB -> 'B';
    CN -> 'N'; CM -> 'M';
  }

singToChar :: SCh ch -> Char
singToChar = toChar . fromSing

instance Show Ch where
  show = show . toChar


instance Show (SCh ch) where
  show = ("Sing @" ++) . show . fromSing

type Str = [Ch]
type SStr :: Str -> Type
type SStr = SList

deriving instance Eq (SStr s)
deriving instance Ord (SStr s)

fromString :: String -> Str
fromString = map fromChar

someFromString :: String -> SomeSing Str
someFromString s = case s of
  "" -> SomeSing SNil
  ch : chs -> case (someFromChar ch, someFromString chs) of
    (SomeSing sCh, SomeSing sChs) -> SomeSing $ SCons sCh sChs

toString :: Str -> String
toString = map toChar

singToString :: SStr s -> String
singToString = map toChar . fromSing




filterChar
  :: MonadError err m
  => err
  -> SCh a
  -> SCh b
  -> e b
  -> m (e a)
filterChar err c1 c2 x = case (c1, c2) of
  {
    (SC_, SC_) -> return x; (SC', SC') -> return x;
    
    (SC1, SC1) -> return x; (SC2, SC2) -> return x; (SC3, SC3) -> return x;
    (SC4, SC4) -> return x; (SC5, SC5) -> return x; (SC6, SC6) -> return x;
    (SC7, SC7) -> return x; (SC8, SC8) -> return x; (SC9, SC9) -> return x;
    (SC0, SC0) -> return x; 
    (SCq, SCq) -> return x; (SCw, SCw) -> return x; (SCe, SCe) -> return x; 
    (SCr, SCr) -> return x; (SCt, SCt) -> return x; (SCy, SCy) -> return x; 
    (SCu, SCu) -> return x; (SCi, SCi) -> return x; (SCo, SCo) -> return x; 
    (SCp, SCp) -> return x; (SCa, SCa) -> return x; (SCs, SCs) -> return x; 
    (SCd, SCd) -> return x; (SCf, SCf) -> return x; (SCg, SCg) -> return x; 
    (SCh, SCh) -> return x; (SCj, SCj) -> return x; (SCk, SCk) -> return x; 
    (SCl, SCl) -> return x; (SCz, SCz) -> return x; (SCx, SCx) -> return x; 
    (SCc, SCc) -> return x; (SCv, SCv) -> return x; (SCb, SCb) -> return x; 
    (SCn, SCn) -> return x; (SCm, SCm) -> return x;
    
    (SCQ, SCQ) -> return x; (SCW, SCW) -> return x; (SCE, SCE) -> return x;
    (SCR, SCR) -> return x; (SCT, SCT) -> return x; (SCY, SCY) -> return x;
    (SCU, SCU) -> return x; (SCI, SCI) -> return x; (SCO, SCO) -> return x;
    (SCP, SCP) -> return x; (SCA, SCA) -> return x; (SCS, SCS) -> return x;
    (SCD, SCD) -> return x; (SCF, SCF) -> return x; (SCG, SCG) -> return x;
    (SCH, SCH) -> return x; (SCJ, SCJ) -> return x; (SCK, SCK) -> return x;
    (SCL, SCL) -> return x; (SCZ, SCZ) -> return x; (SCX, SCX) -> return x;
    (SCC, SCC) -> return x; (SCV, SCV) -> return x; (SCB, SCB) -> return x;
    (SCN, SCN) -> return x; (SCM, SCM) -> return x;

    _ -> throwError err;
  }

example :: SStr '[Ca, Cb, Cc]
example = SCons SCa $ SCons SCb $ SCons SCc SNil

--{-
filterStr
  :: MonadError err m
  => err
  -> SStr a
  -> SStr b
  -> e b
  -> m (e a)
filterStr _ SNil SNil x = return x
filterStr err (SCons a as) (SCons b bs) x = do
  
  xx <- filterStr err as bs (extractParam2 x)
  xxx <- filterChar err a b (extractParam2' $ insertParam2 xx)
  return $ insertParam2' xxx

filterStr err _ _ _ = throwError err



instance GEq SCh where
  geq ch1 ch2 = case (ch1, ch2) of
    {
      (SC_, SC_) -> Just Refl; (SC', SC') -> Just Refl; (SC1, SC1) -> Just Refl;
      (SC2, SC2) -> Just Refl; (SC3, SC3) -> Just Refl; (SC4, SC4) -> Just Refl;
      (SC5, SC5) -> Just Refl; (SC6, SC6) -> Just Refl; (SC7, SC7) -> Just Refl;
      (SC8, SC8) -> Just Refl; (SC9, SC9) -> Just Refl; (SC0, SC0) -> Just Refl;

      (SCq, SCq) -> Just Refl; (SCw, SCw) -> Just Refl; (SCe, SCe) -> Just Refl;
      (SCr, SCr) -> Just Refl; (SCt, SCt) -> Just Refl; (SCy, SCy) -> Just Refl;
      (SCu, SCu) -> Just Refl; (SCi, SCi) -> Just Refl; (SCo, SCo) -> Just Refl;
      (SCp, SCp) -> Just Refl; (SCa, SCa) -> Just Refl; (SCs, SCs) -> Just Refl;
      (SCd, SCd) -> Just Refl; (SCf, SCf) -> Just Refl; (SCg, SCg) -> Just Refl;
      (SCh, SCh) -> Just Refl; (SCj, SCj) -> Just Refl; (SCk, SCk) -> Just Refl;
      (SCl, SCl) -> Just Refl; (SCz, SCz) -> Just Refl; (SCx, SCx) -> Just Refl;
      (SCc, SCc) -> Just Refl; (SCv, SCv) -> Just Refl; (SCb, SCb) -> Just Refl;
      (SCn, SCn) -> Just Refl; (SCm, SCm) -> Just Refl;
      
      (SCQ, SCQ) -> Just Refl; (SCW, SCW) -> Just Refl; (SCE, SCE) -> Just Refl;
      (SCR, SCR) -> Just Refl; (SCT, SCT) -> Just Refl; (SCY, SCY) -> Just Refl;
      (SCU, SCU) -> Just Refl; (SCI, SCI) -> Just Refl; (SCO, SCO) -> Just Refl;
      (SCP, SCP) -> Just Refl; (SCA, SCA) -> Just Refl; (SCS, SCS) -> Just Refl;
      (SCD, SCD) -> Just Refl; (SCF, SCF) -> Just Refl; (SCG, SCG) -> Just Refl;
      (SCH, SCH) -> Just Refl; (SCJ, SCJ) -> Just Refl; (SCK, SCK) -> Just Refl;
      (SCL, SCL) -> Just Refl; (SCZ, SCZ) -> Just Refl; (SCX, SCX) -> Just Refl;
      (SCC, SCC) -> Just Refl; (SCV, SCV) -> Just Refl; (SCB, SCB) -> Just Refl;
      (SCN, SCN) -> Just Refl; (SCM, SCM) -> Just Refl;
      
      _ -> Nothing

    }

instance GEq SStr where

  geq SNil SNil = Just Refl
  geq (SCons ch1 s1) (SCons ch2 s2) = case (geq ch1 ch2, geq s1 s2) of
    (Nothing, _)            -> Nothing
    (_, Nothing)            -> Nothing
    (Just Refl, Just Refl)  -> Just Refl
    
  geq _ _ = Nothing

instance GCompare SCh where
  gcompare ch1 ch2 = case geq ch1 ch2 of
    Just Refl -> GEQ
    Nothing -> case compare (singToChar ch1) (singToChar ch2) of
      LT -> GLT
      GT -> GGT
      EQ -> undefined

instance GCompare SStr where
  gcompare SNil SNil = GEQ
  gcompare (SCons ch1 s1) (SCons ch2 s2) = case gcompare ch1 ch2 of
    GLT -> GLT
    GGT -> GGT
    GEQ -> case gcompare s1 s2 of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
      
  
  gcompare _ SNil = GGT
  gcompare SNil _ = GLT





