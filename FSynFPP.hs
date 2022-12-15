module FSynFPP where

import Data.List

data Column = A' | B' | C' | D' | E' 
            | F' | G' | H' | I' | J'
              deriving (Eq,Ord,Show,Enum)

type Row    = Int 
type Attack = (Column,Row) 

data Ship = Battleship | Frigate | Submarine | Destroyer 
            deriving Show

data Reaction = Missed | Hit Ship | Sunk Ship | LostBattle 
                deriving Show 

type Turn = (Attack,Reaction) 

data Colour   = Red | Yellow | Blue | Green | Orange 
                deriving (Eq,Show,Bounded,Enum)

data Answer   = Black | White deriving (Eq,Show)

type Pattern  = [Colour]
type Feedback = [Answer] 

data Sent = Sent NP VP deriving Show
data NP   = SnowWhite  | Alice  | Dorothy | Goldilocks 
          | LittleMook | Atreyu | Everyone | Someone
          | Wizardland | Wonderland | Camelot | Oz 
          | Merlin | CheshireCat
          | NP1 DET CN | NP2 DET RCN | NP3 DET PCN
          | NP4 PNP | NP5 DET ACN
          deriving Show
data DET  = The | Every | Some | No | Most | AtLeast | AtMost | One | Two | Three
          deriving Show
data CN   = Girl   | Boy   | Princess | Dwarf | Giant 
          | Wizard | Sword | Dagger | Kingdom | Bed | Tower
          | CNP CN PP | ACN ADJ CN -- CNP CN PP new
          deriving Show 
-- ADJ has been moved to the "NEW" section below
data RCN  = RCN1 CN That VP | RCN2 CN That NP TV
          | RCN3 ADJ CN
          deriving Show
data That = That deriving Show
data VP   = Laughed | Cheered | Shuddered | Slept
          | VP1 TV NP | VP2 DV NP NP
          | VP3 AV To INF | VP4 PVP
          | COP1 BE PP | COP2 BE ADJ | COP3 BE NP 
          deriving Show 
data TV   = Loved   | Admired | Helped 
          | Defeated | Caught
          deriving Show 
data BE = Is | Are deriving Show

data DV   = Gave deriving Show
data AV   = Hoped | Wanted deriving Show 
data INF  = Laugh | Sheer | Shudder | INF TINF NP deriving Show
data TINF = Love | Admire | Help | Defeat | Catch 
            deriving Show 
data To   = To deriving Show

-- NEW: --

data TVP = HelpedP | LovedP | AdmiredP deriving Show
data VPP = LaughedP | CheeredP | ShudderedP | SleptP 
          | VPP1 TVP NP  -- TODO rewrite as VPP1 TV PP?
          | TVP NP deriving Show
data PCN = PCN1 CN PP deriving Show
data PNP = PNP1 NP PP deriving Show
data PVP = PVP1 VPP PP | PVP2 TVP NP PP deriving Show
data PP = Here | There | PP1 PR NP deriving Show
data PR = In | For | At | On | Over | Under | EmptyPR | Behind deriving (Show, Eq)

data ADJ = Dwarven | Human | Female | Male | Sharp | Fake deriving Show
data ACN = ACN1 ADJ CN deriving Show

-- /NEW --

data Form = Prop String | Ng Form | Cnj [Form] | Dsj [Form] 
            deriving Eq

instance Show Form where
 show (Prop name) = name 
 show (Ng  f)  = '-': show f
 show (Cnj fs) = '&': show fs
 show (Dsj fs) = 'v': show fs

form1, form2 :: Form
form1 = Cnj [Prop "p", Ng (Prop "p")]
form2 = Dsj [Prop "p1", Prop "p2", Prop "p3", Prop "p4"]

type Name     = String 
type Index    = [Int]
data Variable = Variable Name Index deriving (Eq,Ord)

instance Show Variable where 
  show (Variable name [])  = name
  show (Variable name [i]) = name ++ show i
  show (Variable name is ) = name ++ showInts is
     where showInts []     = "" 
           showInts [i]    = show i  
           showInts (i:is) = show i ++ "_" ++ showInts is

x, y, z :: Variable
x = Variable "x" []
y = Variable "y" []
z = Variable "z" []

data Formula a = Atom String [a]
               | Eq a a
               | Neg  (Formula a)
               | Impl (Formula a) (Formula a) 
               | Equi (Formula a) (Formula a)
               | Conj [Formula a]
               | Disj [Formula a] 
               | Forall Variable (Formula a)
               | Exists Variable (Formula a)
               deriving Eq

instance Show a => Show (Formula a) where 
  show (Atom s [])   = s
  show (Atom s xs)   = s ++ show xs 
  show (Eq t1 t2)    = show t1 ++ "==" ++ show t2
  show (Neg form)    = '~' : (show form)
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equi f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"
  show (Conj [])     = "true" 
  show (Conj fs)     = "conj" ++ show fs 
  show (Disj [])     = "false" 
  show (Disj fs)     = "disj" ++ show fs 
  show (Forall v f)  = "A " ++  show v ++ (' ' : show f)
  show (Exists v f)  = "E " ++  show v ++ (' ' : show f)

formula0 = Atom "R" [x,y]
formula1 = Forall x (Atom "R" [x,x])
formula2 = Forall x 
            (Forall y
              (Impl (Atom "R" [x,y]) (Atom "R" [y,x])))

data Term = Var Variable | Struct String [Term] 
            deriving (Eq,Ord)

instance Show Term where 
  show (Var v)       = show v 
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

tx, ty, tz :: Term 
tx = Var x
ty = Var y
tz = Var z

isVar :: Term -> Bool
isVar (Var _) = True
isVar _       = False

varsInTerm :: Term -> [Variable]
varsInTerm (Var v)       = [v]
varsInTerm (Struct s ts) = varsInTerms ts

varsInTerms :: [Term] -> [Variable]
varsInTerms = nub . concat . map varsInTerm

