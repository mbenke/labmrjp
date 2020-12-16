module FlowBere where
import RistrettoBere
import qualified Data.Set as Set
import Data.Set(Set,(\\),union)
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Monad.State

arg = VParam
loc = VLocal
infix 2 $=
($=) = Istore

-- Usage: n $=- v1 $ v2
infix 2 $=-
($=-) = Isub

infix 2 $=+
($=+) = Iadd

{- 
 x1 = a1; x2 = a2; 
 x3 = x1; x1 = x2; x2 = x3; 
 x3 = x1-x2; x4 = x1-x3; x5 = x3+x4; x6 = x5+x4
-}
example1 = [ 1 $= a 1
           , 2 $= a 2
           , 3 $= x 1
           , 1 $= x 2
           , 2 $= x 3
           , 3 $=- (x 1) $ (x 2)
           , 4 $=- (x 1) $ (x 3)
           , 5 $=+ (x 3) $ (x 4)
           , 6 $=+ (x 5) $ (x 4)
           , Iretint (x 6)
           ] where
             a = VParam
             x = VLocal    

example2 = [ 1 $= c 7
           , Iretint (c 42)
           ] where c = VConst

valVars :: Val -> [Int]
valVars (VLocal n) = [n]
valVars _ = []

liveGen :: Instr -> [Int]
liveGen (Iretvoid) = []
liveGen (Iretint v) = valVars v
liveGen (Icall _ vs) = concatMap valVars vs
liveGen (Istore n v1) = valVars v1 
liveGen (Iadd n v1 v2) = concatMap valVars [v1, v2]
liveGen (Isub n v1 v2) = concatMap valVars [v1, v2]

liveKill (Istore n v1) = [n] 
liveKill (Iadd n v1 v2) = [n]
liveKill (Isub n v1 v2) = [n]
liveKill _ = []

liveOut i liveIn = union (liveIn \\ kill) gen where
    gen  = Set.fromList $ liveGen i
    kill = Set.fromList $ liveKill i

live :: [Instr] -> Set Int
live [] = Set.empty
live (i:is) = liveOut i (live is)

liveSteps :: [Instr] -> [(Instr,Set Int)]
liveSteps = liveSteps' Set.empty

liveSteps' :: Set Int -> [Instr] -> [(Instr,Set Int)]
liveSteps' atEnd is = go is where
  go   []   = []
  go  [i]   = [(i,liveOut i liveIn)] where liveIn = atEnd
  go (i:is) =  (i,liveOut i liveIn):rest where
    rest = go is  -- nonempty
    liveIn = snd (head rest)
