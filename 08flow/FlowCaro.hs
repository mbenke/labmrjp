module FlowCaro where
import RistrettoCaro
import PrintCaro
import qualified Data.Set as Set
import Data.Set(Set,(\\),union)
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Monad.State
import Data.Functor((<$>))
infix 2 $=
n $= v = Iassign n (EVal v)

-- Usage: n $=- v1 $ v2
infix 2 $=-
($=-) n v1 v2 = Iassign n (EOp OpSub v1 v2)

infix 2 $=+
($=+) n v1 v2 = Iassign n (EOp OpAdd v1 v2)

retint = Iret . Just

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
           , retint (x 6)
           ] where
             a = VParam
             x = VLocal

example2 = [ 1 $= c 7
           , retint (c 42)
           ] where c = VConst

-- This example checks for invalidation after reassignment
{- x1 = a1+a2; x2 = x1; x1 = a1 - a2; x3 = x2; ret x3 -}
example3 = [ 1 $=+ a 1 $ a 2
           , 2 $= x 1
           , 1 $=- a 1 $ a 2
           , 3 $= x 2
           , retint (x 3)
           ] where
             a = VParam
             x = VLocal

valVars :: Val -> [Int]
valVars (VLocal n) = [n]
valVars _ = []

liveGen :: Instr -> [Int]
liveGen (Iret Nothing) = []
liveGen (Iret (Just v)) = valVars v
liveGen (Iassign n e) = exprVars e
liveGen Inop = []

exprVars (EVal v) = valVars v
exprVars (ECall _ vs) = concatMap valVars vs
exprVars (EOp _ v1 v2) = concatMap valVars [v1, v2]

liveKill (Iassign n v1) = [n]
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
