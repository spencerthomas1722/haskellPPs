module Model where 

import Data.List
import FSynFPP

data Entity = A | B | D | E | F | G
            | M | O | R | S | T 
            | V | W | X | Y | Unspec
            | WzL | WoL | Cam | Tow
            | Mer | CC | WK | WQ | RK | RQ
            | MB | PB | BB
            | B1 | B2 | B3
            | Gan | WOz | GF | MF | DK
            | AM | DM
     deriving (Eq,Show,Bounded,Enum)

entities :: [Entity]
entities =  [minBound..maxBound] 

snowWhite, alice, dorothy, goldilocks, littleMook, atreyu, wizardland, wonderland, oz, camelot, redking, redqueen, whiteking, whitequeen, mamabear, papabear, babybear :: Entity

snowWhite  = S
alice      = A
dorothy    = D
goldilocks = G 
littleMook = M
atreyu     = Y
merlin     = Mer
cheshirecat = CC
wizardland = WzL
wonderland = WoL
oz = O
camelot = Cam
redking = RK
redqueen = RQ
whiteking = WK
whitequeen = WQ
mamabear = MB
papabear = PB
babybear = BB
gandalf  = Gan
wizardofoz = WOz
grizzlyforest = GF
magicforest = MF
doorknob      = DK

type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \ x -> elem x xs

girl, boy, woman, man, king, queen, princess, dwarf, giant, wizard, sword, dagger, kingdom, bed, tower, bear, gift
                                         :: OnePlacePred

girl     = list2OnePlacePred [S,A,D,G]
boy      = list2OnePlacePred [M,Y]
woman    = list2OnePlacePred [RQ,WQ,E,T,R,W,V]
man      = list2OnePlacePred [RK,WK,B,R,V,Mer,Gan,WOz]
king     = list2OnePlacePred [RK,WK]
queen    = list2OnePlacePred [RQ,WQ]
princess = list2OnePlacePred [S,E]
dwarf    = list2OnePlacePred [B,R]
giant    = list2OnePlacePred [T]
wizard   = list2OnePlacePred [W,V,Mer,Gan,WOz]
sword    = list2OnePlacePred [F]
dagger   = list2OnePlacePred [X]
kingdom  = list2OnePlacePred [WzL,Cam]
bed      = list2OnePlacePred [B1,B2,B3]
tower    = list2OnePlacePred [Tow]
bear     = list2OnePlacePred [MB,PB,BB]
mama     = list2OnePlacePred [MB]
papa     = list2OnePlacePred [PB]
baby     = list2OnePlacePred [BB]
forest   = list2OnePlacePred [GF,MF]
gift     = list2OnePlacePred [AM,DM,X]
knob     = list2OnePlacePred [DK]

child, person, male, female, thing :: OnePlacePred

child  = \ x -> (girl x  || boy x)
adult  = \ x -> (person x) && not (child x)
person = \ x -> (child x || adult x || princess x || dwarf x 
                         || giant x    || wizard x) 
male   = \ x -> (man x || boy x) 
female = \ x -> (woman x || girl x)
thing  = \ x -> (not (person x || x == Unspec)) || knob x
place  = \ x -> (forest x || kingdom x)

-- New/altered stuff --

dwarven = \ x -> dwarf x
human   = \ x -> not (dwarf x || giant x || thing x)
sharp   = list2OnePlacePred [X]
fake    = list2OnePlacePred []
royal   = \ x -> ((queen x || king x || princess x) || any (\ p -> (forNP x p || ofNP x p)) (filter royal entities))  -- returns True if belongs to a member of royalty, or to one of their belongings
fuzzy   = \ x -> bear x || elem x [W]

firstOfTriple :: (a, b, c) -> a
firstOfTriple (x, _, _) = x

firstTwoOfQuadruple :: (a, b, c, d) -> (a, b)
firstTwoOfQuadruple (x, y, _, _) = (x, y)

firstTwoOfQuintuple ::(a, b, c, d, e) -> (a, b)
firstTwoOfQuintuple (x, y, _, _, _) = (x, y)
 
firstThreeOfQuintuple :: (a, b, c, d, e) -> (a, b, c)
firstThreeOfQuintuple (x, y, z, _, _) = (x, y, z)

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

curry4 :: ((a,b,c,d) -> e) -> a -> b -> c -> d -> e
curry4 f x y z w = f (x,y,z,w)

curry5 :: ((a,b,c,d,e) -> f) -> a -> b -> c -> d -> e -> f
curry5 f x y z w u = f (x,y,z,w,u)

laugh, cheer, shudder :: OnePlacePred
laughList   = [(A,In,WoL),(G,EmptyPR,Unspec),(E,EmptyPR,Unspec),(W,In,WzL),(V,In,O)]
sleepList   = [(W,In,WzL),(G,In,B1),(G,In,B2),(G,In,B3),(MB,In,B1),(PB,In,B2),(BB,In,B3)]
cheerList   = [(M,EmptyPR,Unspec),(D,In,O)]
shudderList = [(S,EmptyPR,Unspec),(W,In,WzL),(Gan,EmptyPR,Unspec),(Mer,Under,Cam)]
cryList     = [(D,In,O),(MB,EmptyPR,Unspec)]

laugh   = \ x -> any (\ tr -> firstOfTriple tr == x) laughList
-- laugh   = list2OnePlacePred [A,G,E,W]
sleep   = \ x -> any (\ tr -> firstOfTriple tr == x) sleepList
cheer   = \ x -> any (\ tr -> firstOfTriple tr == x) cheerList
shudder = \ x -> any (\ tr -> firstOfTriple tr == x) shudderList
cry     = \ x -> any (\ tr -> firstOfTriple tr == x) cryList

sleepPP, cheerPP, shudderPP :: PR -> Entity -> Entity -> Bool
laughPP   = \ pr subj loc -> curry3 (`elem` laughList) subj pr loc
sleepPP   = \ pr subj loc -> curry3 (`elem` sleepList) subj pr loc
cheerPP   = \ pr subj loc -> curry3 (`elem` cheerList) subj pr loc
shudderPP = \ pr subj loc -> curry3 (`elem` shudderList) subj pr loc
cryPP     = \ pr subj loc -> curry3 (`elem` cryList) subj pr loc

love, admire, help, defeat, kill :: TwoPlacePred

loveList   = [(Y,E,EmptyPR,Unspec),(B,S,EmptyPR,Unspec),(R,S,EmptyPR,Unspec), (MB,PB,EmptyPR,Unspec),(PB,MB,EmptyPR,Unspec),(MB,BB,EmptyPR,Unspec),(BB,MB,EmptyPR,Unspec), (PB,BB,EmptyPR,Unspec),(BB,PB,EmptyPR,Unspec),(Gan,Mer,EmptyPR,Unspec),(Mer,Gan,EmptyPR,Unspec)]
-- loveList has no PRs/locations because these people's love transcends time and space ^_^
admireList = [(x,G,EmptyPR,Unspec) | x <- entities, person x] ++ [(BB,MB,EmptyPR,Unspec), (BB,PB,EmptyPR,Unspec)]
helpList   = [(W,W,EmptyPR,Unspec),(V,V,EmptyPR,Unspec),(S,B,In,WzL),(D,M,In,WzL),(CC,A,In,WoL),(WOz,D,In,O),(B,S,In,MF),(R,S,In,MF)]
defeatList = [(x,y,EmptyPR,Unspec) | x <- entities, y <- entities, dwarf x && giant y]
                    ++ [(A,W,In,WoL),(A,V,In,WoL),(PB,G,In,GF),(MB,G,In,GF),
                        (BB,G,In,GF)]
seeList    = [(x,A,In,WoL) | x <- filter (\ w -> inNP w WoL) entities] ++ [(x,y,In,GF) | x <- entities, y <- entities, bear x && bear y] ++ [(E,Y,EmptyPR,Unspec),(Y,E,EmptyPR,Unspec),(WOz,D,In,O),(V,W,Over,WzL),(W,V,In,WzL)]
killList = [(Y,T,F,EmptyPR,Unspec),(Unspec,D,X,In,O),(Unspec,M,Unspec,EmptyPR,Unspec),
            (MB,G,MB,In,GF),(PB,G,PB,In,GF),(MB,G,MB,With,PB),(PB,G,BB,With,MB)]

love   = \ x y -> any (\ quad -> firstTwoOfQuadruple quad == (x,y)) loveList
admire = \ x y -> any (\ quad -> firstTwoOfQuadruple quad == (x,y)) admireList
help   = \ x y -> any (\ quad -> firstTwoOfQuadruple quad == (x,y)) helpList
defeat = \ x y -> any (\ quad -> firstTwoOfQuadruple quad == (x,y)) defeatList
see    = \ x y -> any (\ quad -> firstTwoOfQuadruple quad == (x,y)) seeList
kill   = \ x y -> any (\ quin -> firstTwoOfQuintuple quin == (x,y)) killList
kill3  = \ x y z -> any (\quin -> firstThreeOfQuintuple quin == (x,y,z)) killList

lovePP, admirePP, helpPP, defeatPP :: Entity -> Entity -> PR -> Entity -> Bool
lovePP   = curry4 (`elem` loveList)
admirePP = curry4 (`elem` admireList)
helpPP   = curry4 (`elem` helpList)
-- "help x y" itself is True if there is some preposition PR and location "loc" 
--  where (x,y,PR) is in this list
defeatPP = curry4 (`elem` defeatList)
seePP    = curry4 (`elem` seeList)
-- X killed Y [PR] [Loc] is true if there is any weapon such that...
killPP   = \ x y z w -> any (\ weapon -> elem (x,y,weapon,z,w) killList) entities
                        || (z == With && kill3 x y w)

give :: ThreePlacePred

giveList = [(T,S,X,In,Unspec),(A,E,S,In,Unspec),(E,Y,AM,In,MF),(Unspec,A,DM,EmptyPR,Unspec)]

give = \ x y z -> any (\ quin -> firstThreeOfQuintuple quin == (x,y,z)) giveList

givePP = curry5 (`elem` giveList)

passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> r Unspec x

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x 

inNP, forNP, ofNP, underNP :: TwoPlacePred
inNP   = curry (`elem` [(V,Tow),(W,WzL),(Mer,Cam),(A,WoL),(CC,WoL),(Tow,WzL)])
forNP  = curry (`elem` [(X, E)])
fromNP = curry (`elem` [(V,WzL),(W,WzL),(WOz,O),(B,MF),(R,MF),(BB,GF)])
ofNP   = \ x y -> (elem (x,y) [(Mer,Cam),(MB,GF),(PB,GF),(RQ,WoL),(RK,WoL),(WQ,WoL),(WK,WoL)]) || fromNP x y
underNP = curry (`elem` [(Mer,Cam)])
overNP = curry (`elem` [(V,WzL)])

betweenNP :: ThreePlacePred
betweenNP = curry3 (`elem` [(A,WQ,RQ),(RQ,A,RK),(WQ,A,WK),(WK,WQ,CC)]) -- implies reverse as well; see TCOM

smallToGiant = \ x -> not (place x || giant x)
smallToHuman = \ x -> dwarf x
bigToGiant   = \ x -> not (person x)
bigToHuman   = \ x -> not (person x) || giant x