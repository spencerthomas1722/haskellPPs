module Model where 

import Data.List
import FSynFPP

data Entity = A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
            | WzL | WoL | Cam
            | Mer
            | B1 | B2 | B3
     deriving (Eq,Show,Bounded,Enum)

entities :: [Entity]
entities =  [minBound..maxBound] 

snowWhite, alice, dorothy, goldilocks, littleMook, atreyu, wizardland, wonderland, oz, camelot :: Entity

snowWhite  = S
alice      = A
dorothy    = D
goldilocks = G 
littleMook = M
atreyu     = Y
merlin     = Mer
wizardland = WzL
wonderland = WoL
oz = O
camelot = Cam

type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \ x -> elem x xs

girl, boy, princess, dwarf, giant, wizard, sword, dagger, kingdom, bed, tower
                                         :: OnePlacePred

girl     = list2OnePlacePred [S,A,D,G]
boy      = list2OnePlacePred [M,Y]
princess = list2OnePlacePred [E]
dwarf    = list2OnePlacePred [B,R]
giant    = list2OnePlacePred [T]
wizard   = list2OnePlacePred [W,V,Mer]
sword    = list2OnePlacePred [F]
dagger   = list2OnePlacePred [X]
kingdom  = list2OnePlacePred [WzL, O, Cam]
bed      = list2OnePlacePred [B1,B2,B3]
tower    = list2OnePlacePred []

child, person, man, woman, male, female, thing :: OnePlacePred

child  = \ x -> (girl x  || boy x)
person = \ x -> (child x || princess x || dwarf x 
                         || giant x    || wizard x) 
man    = \ x -> (dwarf x || giant x || wizard x) 
woman  = \ x -> princess x 
male   = \ x -> (man x || boy x) 
female = \ x -> (woman x || girl x)
thing  = \ x -> not (person x || x == Unspec)

-- New/altered stuff --

firstOfTriple :: (a, b, c) -> a
firstOfTriple (x, _, _) = x

firstTwoOfQuadruple :: (a, b, c, d) -> (a, b)
firstTwoOfQuadruple (x, y, _, _) = (x, y)

firstThreeOfQuintuple :: (a, b, c, d, e) -> (a, b, c)
firstThreeOfQuintuple (x, y, z, _, _) = (x, y, z)

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

curry4 :: ((a,b,c,d) -> e) -> a -> b -> c -> d -> e
curry4 f x y z w = f (x,y,z,w)

curry5 :: ((a,b,c,d,e) -> f) -> a -> b -> c -> d -> e -> f
curry5 f x y z w u = f (x,y,z,w,u)

laugh, cheer, shudder :: OnePlacePred
laughList = [(A,In,WoL),(G,EmptyPR,Unspec),(E,EmptyPR,Unspec),(W,In,WzL)]
sleepList = [(W,In,WzL),(G,In,B1),(G,In,B2),(G,In,B3)]
cheerList = [(M,In,Unspec), (D,In,Unspec)]
shudderList = [(S,In,Unspec)]

laugh   = \ x -> any (\ tr -> firstOfTriple tr == x) laughList
-- laugh   = list2OnePlacePred laughList
cheer   = \ x -> any (\ tr -> firstOfTriple tr == x) cheerList
sleep   = \ x -> any (\ tr -> firstOfTriple tr == x) sleepList
shudder = \ x -> any (\ tr -> firstOfTriple tr == x) shudderList

sleepPP, cheerPP, shudderPP :: PR -> Entity -> Entity -> Bool
laughPP   = \ pr subj loc -> curry3 (`elem` laughList) subj pr loc
sleepPP   = \ pr subj loc -> curry3 (`elem` sleepList) subj pr loc
cheerPP   = \ pr subj loc -> curry3 (`elem` cheerList) subj pr loc
shudderPP = \ pr subj loc -> curry3 (`elem` shudderList) subj pr loc

love, admire, help, defeat :: TwoPlacePred

loveList   = [(Y,E,EmptyPR,Unspec),(B,S,EmptyPR,Unspec),(R,S,EmptyPR,Unspec)]
admireList = [(x,G,EmptyPR,Unspec) | x <- entities, person x]
helpList   = [(W,W,EmptyPR,Unspec),(V,V,EmptyPR,Unspec),(S,B,In,WzL),(D,M,In,WzL)]
defeatList = [(x,y,EmptyPR,Unspec) | x <- entities, y <- entities, dwarf x && giant y]
                    ++ [(A,W,In,WoL),(A,V,In,WoL)]

love   = \ x y -> any (\ quad -> firstTwoOfQuadruple quad == (x,y)) loveList
admire = \ x y -> any (\ quad -> firstTwoOfQuadruple quad == (x,y)) admireList
help   = \ x y -> any (\ quad -> firstTwoOfQuadruple quad == (x,y)) helpList
defeat = \ x y -> any (\ quad -> firstTwoOfQuadruple quad == (x,y)) defeatList

lovePP, admirePP, helpPP, defeatPP :: Entity -> Entity -> PR -> Entity -> Bool
lovePP   = curry4 (`elem` loveList)
admirePP = curry4 (`elem` admireList)
helpPP   = curry4 (`elem` helpList)
-- "help x y" itself is True if there is some preposition PR and location "loc" 
--  where (x,y,PR) is in this list
defeatPP = curry4 (`elem` defeatList)

give, kill :: ThreePlacePred

giveList = [(T,S,X,In,Unspec),(A,E,S,In,Unspec)]
killList = [(Y,T,F,EmptyPR,Unspec),(Unspec,D,X,In,O), (Unspec,M,Unspec,EmptyPR,Unspec)]

give = \ x y z -> any (\ quin -> firstThreeOfQuintuple quin == (x,y,z)) giveList
kill = \ x y z -> any (\ quin -> firstThreeOfQuintuple quin == (x,y,z)) killList

givePP = curry5 (`elem` giveList)
killPP = curry5 (`elem` killList)

passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> r Unspec x

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x 

inNP, forNP :: TwoPlacePred
inNP   = curry (`elem` [(W, WzL), (V, WzL), (Mer, Cam)])
-- inNP = W -> WzL -> True
forNP  = curry (`elem` [(X, E)])