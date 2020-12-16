{-# LANGUAGE MultiParamTypeClasses #-}
module LlvmState where
import Control.Monad.State
import qualified Data.Map as Map
import Control.Applicative

import LlvmAsm
import Fresh
-- import MonadUtils

type GenM a = State LState a
evalGen :: GenM a -> a
evalGen m = evalState m initState

runGen :: GenM a -> (a,LState)
runGen m = runState m initState

data LState = LState {
  nextid :: Int,                 -- first free id
  llocals :: Map.Map String Int, -- map local to addr
  rcode :: [LInstr]              -- code so far, reversed
}

initState = LState { 
  nextid = 1, 
  llocals = Map.empty,
  rcode = []
  }

instance HasFresh Int LState where
  nextFresh s = (i, s { nextid = i+1 }) where
    i = nextid s
    
freshInt :: GenM Int
freshInt = fresh

freshLabel :: GenM Label
freshLabel = fresh

emit :: LInstr -> GenM ()
emit i = do
  st <- get
  put st { rcode = i:rcode st}

genTemp :: GenM Address
genTemp = Temp <$> fresh

findLocal :: String -> GenM (Maybe Int)
findLocal s = do
  env <- gets llocals
  return $ Map.lookup s env

addLocal :: String -> Int -> GenM Address
addLocal s n = do
  modify (\st -> st { llocals = Map.insert s n (llocals st) })
  let a = Local n
  emit $ alloca a
  return a

getLocal :: String -> GenM Address
getLocal s = findLocal s >>= \r -> case r of
  Just n -> return $ Local n
  Nothing -> do
    n <- fresh
    a <- addLocal s n
    return a
