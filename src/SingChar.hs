{-# LANGUAGE
    TemplateHaskell
  , KindSignatures
  , StandaloneKindSignatures
  , DataKinds
  , GADTs
  , TypeFamilies
  , StandaloneDeriving

  , TypeSynonymInstances
  , FlexibleInstances
#-}

module SingChar where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Kind ( Type )

data Ch
  = C_ | C' | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C0
  | Cq | Cw | Ce | Cr | Ct | Cy | Cu | Ci | Co | Cp | Ca | Cs | Cd | Cf | Cg 
  | Ch | Cj | Ck | Cl | Cz | Cx | Cc | Cv | Cb | Cn | Cm 
  | CQ | CW | CE | CR | CT | CY | CU | CI | CO | CP | CA | CS | CD | CF | CG 
  | CH | CJ | CK | CL | CZ | CX | CC | CV | CB | CN | CM

genSingletons [''Ch]


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
