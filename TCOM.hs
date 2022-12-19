module TCOM where 

import Data.List
import FSynFPP
import Model

allNum, noNum :: Int -> Int -> Bool
allNum = \ m n -> m == 0
noNum  = \ m n -> n == 0 

atleastNum, atmostNum :: Int -> Int -> Int -> Bool
atleastNum k = \ m n -> n >= k
atmostNum  k = \ m n -> n <= k

atleast2butnotall :: Int -> Int -> Bool
atleast2butnotall = \ m n -> m > 0 && n >= 2

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) =  f x y z 

rel3 :: Entity -> Entity -> Entity -> Bool
rel3 D x y = love x y 
rel3 E x y = not (love x y)
rel3 _ _ _ = False 

intSent :: Sent -> Bool 
intSent (Sent np vp) = (intNP np) (intVP vp)

intNP :: NP -> (Entity -> Bool) -> Bool
intNP SnowWhite     = \ p -> p snowWhite 
intNP Alice         = \ p -> p alice
intNP Dorothy       = \ p -> p dorothy
intNP Goldilocks    = \ p -> p goldilocks
intNP LittleMook    = \ p -> p littleMook
intNP Atreyu        = \ p -> p atreyu
intNP Wizardland    = \ p -> p wizardland
intNP Wonderland    = \ p -> p wonderland
intNP Oz            = \ p -> p oz
intNP Camelot       = \ p -> p camelot
intNP Merlin        = \ p -> p merlin
intNP CheshireCat   = \ p -> p cheshirecat
intNP RedKing       = \ p -> p redking
intNP RedQueen      = \ p -> p redqueen
intNP WhiteKing     = \ p -> p whiteking
intNP WhiteQueen    = \ p -> p whitequeen
intNP Gandalf       = \ p -> p gandalf
intNP MamaBear      = \ p -> p mamabear
intNP PapaBear      = \ p -> p papabear
intNP BabyBear      = \ p -> p babybear
intNP (NP1 det cn)  = (intDET det) (intCN cn) 
intNP (NP2 det rcn) = (intDET det) (intRCN rcn) 
intNP (NP3 det pcn) = (intDET det) (intPCN pcn)  -- cn with pp adjunct
intNP (NP4 pnp) = intPNP pnp  -- full np with pp adjunct
intNP (NP5 det acn) = (intDET det) (intACN acn)

intVP :: VP -> Entity -> Bool 
intVP Laughed       = \ x -> laugh x
intVP Cheered       = \ x -> cheer x 
intVP Slept         = \ x -> sleep x
intVP Shuddered     = \ x -> shudder x 
intVP (COP1 be pp)  = \ subj -> intPP pp subj
intVP (COP2 be adj) = \ subj -> intADJ adj subj
intVP (COP3 be np)  = \ subj -> intNP np (== subj)
intVP (VP1 tv np)   = 
  \ subj -> intNP np (\ obj -> intTV tv subj obj)
intVP (VP2 dv np1 np2) = 
  \ subj -> intNP np1 (\ iobj -> intNP np2 (\ dobj -> 
                         intDV dv subj iobj dobj))
intVP (VP4 pvp)     = intPVP pvp

intTV :: TV -> Entity -> Entity -> Bool
intTV Loved    = \ x y -> love x y
intTV Admired  = \ x y -> admire x y
intTV Helped   = \ x y -> help x y
intTV Defeated = \ x y -> defeat x y

intDV :: DV -> Entity -> Entity -> Entity -> Bool
intDV Gave   = \ x y z -> give x y z
intDV Killed = \ x y z -> kill x y z

intCN :: CN -> Entity -> Bool
intCN Girl     = \ x -> girl x
intCN Boy      = \ x -> boy x
intCN King     = \ x -> king x
intCN Queen    = \ x -> queen x
intCN Princess = \ x -> princess x
intCN Dwarf    = \ x -> dwarf x 
intCN Giant    = \ x -> giant x 
intCN Wizard   = \ x -> wizard x 
intCN Sword    = \ x -> sword x
intCN Dagger   = \ x -> dagger x
intCN Kingdom  = \ x -> kingdom x
intCN Bed      = \ x -> bed x
intCN Tower    = \ x -> tower x
intCN Bear     = \ x -> bear x

intDET :: DET -> 
         (Entity -> Bool) -> (Entity -> Bool) -> Bool

intDET Some p q = any q (filter p entities)

intDET Every p q = all q (filter p entities)

intDET The p q = singleton plist && q (head plist) 
          where 
              plist = filter p entities
              singleton [x] = True 
              singleton  _  = False

intDET No p q = not (intDET Some p q) 

intDET Most p q = length pqlist > length (plist \\ qlist)
    where 
         plist  = filter p entities 
         qlist  = filter q entities 
         pqlist = filter q plist

intDET One p q = intDETN AtLeast 1 p q
intDET Two p q = intDETN AtLeast 2 p q
intDET Three p q = intDETN AtLeast 3 p q

intDETN :: DET -> 
         Int -> (Entity -> Bool) -> (Entity -> Bool) -> Bool

intDETN AtLeast n p q = length (filter q (filter p entities)) >= n
intDETN AtMost n p q = length (filter q (filter p entities)) <= n

intRCN :: RCN -> Entity -> Bool
intRCN (RCN1 cn _ vp) = 
       \ e -> ((intCN cn e) && (intVP vp e))

intRCN (RCN2 cn _ np tv) = 
   \ e -> ((intCN cn e) && 
           (intNP np (\ subj -> (intTV tv subj e))))

-- NEW STUFF: --
intPCN :: PCN -> (Entity -> Bool)
intPCN (PCN1 cn pp) = \ x -> ((intCN cn x) && (intPP pp x))

intPNP :: PNP -> ((Entity -> Bool) -> Bool)
intPNP (PNP1 np pp) = \ pred -> (intNP loc) (\ l -> intDET inDet (\ s -> intPR pr s l && intCN inCn s) pred)
     where (PP1 pr loc) = pp
           (NP1 inDet inCn) = np

intPVP :: PVP -> Entity -> Bool
intPVP (PVP1 vp pp) = \ subj -> (intNP loc) (((intVPP vp) pr) subj)
     where (PP1 pr loc) = pp
intPVP (PVP2 tvp np pp) = \ subj -> (intNP loc) (((intVPP (VPP1 tvp np)) pr) subj)
     where (PP1 pr loc) = pp

intVPP :: VPP -> PR -> Entity -> Entity -> Bool
intVPP LaughedP   = \ pr -> \ subj -> \ loc -> laughPP pr subj loc
intVPP CheeredP   = \ pr -> \ subj -> \ loc -> cheerPP pr subj loc 
intVPP SleptP     = \ pr -> \ subj -> \ loc -> sleepPP pr subj loc
intVPP ShudderedP = \ pr -> \ subj -> \ loc -> shudderPP pr subj loc 
intVPP (VPP1 tvp np) = \ pr -> \ subj -> \loc -> intNP np (\ obj -> intTVP tvp subj obj pr loc)
 
intTVP :: TVP -> Entity -> Entity -> PR -> Entity -> Bool
intTVP HelpedP = \ subj obj pr loc -> helpPP subj obj pr loc

intPP :: PP -> (Entity -> Bool)
intPP (PP1 pr n) = \ x -> intNP n (\ loc -> intPR pr x loc) -- adapted from intVP
-- cf. intVP (VP1 tv np) = \ subj -> intNP np (\ obj -> intTV tv subj obj)
intPP (PP2 pr n1 _ n2) = \ x -> intNP n2 (\ loc2 -> intNP n1 (\ loc1 -> intTPR pr x loc1 loc2))
--                       \ x -> intNP n1 (intNP n2 (\ loc1 -> \ loc2 -> intDV dv x loc1 loc2))

intPR :: PR -> Entity -> Entity -> Bool -- adapted from intTV
intPR In x y | inNP x y = True
             | otherwise = any (\ l -> inNP x l) (filter (\ z -> inNP x z) entities)
intPR For x y = forNP x y
intPR Of x y = ofNP x y
intPR Under x y = underNP x y

entityPairs = [(x, y) | x <- entities, y <- entities]

intTPR :: TPR -> Entity -> Entity -> Entity -> Bool
intTPR Between x y z | betweenNP x y z = True
                     | betweenNP x z y = True
                     | any (\ (u,v) -> any (\ l -> intTPR Between u l x) (filter (\ w -> betweenNP u x w || betweenNP u w x) entities)) (filter (\ (u,v) -> betweenNP x u v) entityPairs) = True
                     | otherwise = False
intTPR Betwixt x y z = intTPR Between x y z

intACN :: ACN -> (Entity -> Bool)
intACN (ACN1 adj cn) = \ x -> ((intCN cn x) && (intADJ adj x))

intADJ :: ADJ -> Entity -> Bool
intADJ Dwarven = \ x -> dwarf x
intADJ Human   = \ x -> human x
intADJ Female  = \ x -> female x
intADJ Male    = \ x -> male x
intADJ Sharp   = \ x -> sharp x
intADJ Fake    = \ x -> fake x  -- had to add this since van Eijck and Unger did --- currently leads to an empty list
intADJ Mama    = \ x -> mama x
intADJ Papa    = \ x -> papa x
intADJ Baby    = \ x -> baby x