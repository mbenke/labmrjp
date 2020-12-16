{-# LANGUAGE MultiParamTypeClasses #-}
module JvmState where

import qualified Data.Map as Map
import Control.Monad.State

import Jasmin
import Fresh

type GenM a = State JState a

runGen :: GenM a -> (a,JState)
runGen m = runState m (initState "Instant")

data JState = JState { 
  currentClass :: String,        -- name of the current class 
  depth :: Int,                  -- current stack depth
  maxdepth :: Int,               -- max stack depth
  nextlocal :: Int,              -- first free local
  jlocals :: Map.Map String Int,  -- map local to addr
  rcode :: [JInstr],              -- code so far, reversed
  nextid :: Int
  }
              
instance HasFresh Int JState where
  nextFresh s = (i, s { nextid = i+1 }) where
    i = nextid s
    
freshInt :: GenM Int
freshInt = fresh

freshLabel :: GenM Label
freshLabel = do
  i <- freshInt
  return $ "L" ++ show i
  
initState :: String -> JState
initState cname = JState {
  currentClass = cname, 
  depth = 0,
  maxdepth = 0, 
  nextlocal = 2,
  jlocals = Map.empty, 
  rcode = [],
  nextid = 0
  }

emit :: JInstr -> GenM ()
emit i = do
  st <- get
  put st { rcode = i:rcode st}

getCurrentClass :: GenM String
getCurrentClass = gets currentClass

nextLocal :: GenM Int
nextLocal = do
  st <- get
  let l = nextlocal st
  put st { nextlocal = l + 1 }
  return l
  
findLocal :: String -> GenM (Maybe Int)
findLocal s = do
  env <- gets jlocals
  return $ Map.lookup s env
  
addLocal :: String -> Int -> GenM ()
addLocal s n = modify (\st -> st { jlocals = Map.insert s n (jlocals st) })

getLocal :: String -> GenM Int
getLocal s = findLocal s >>= \r -> case r of
  Just n -> return n
  Nothing -> do
    n <- nextLocal
    addLocal s n
    return n
                   
incDepth :: Int -> JState -> JState
incDepth i s = s { depth = depth s + i }

putmaxdepth :: Int -> JState -> JState
putmaxdepth i s = s { maxdepth = i }

incStack :: Int -> GenM ()
incStack i = do
  modify (incDepth i)
  d <- gets depth
  m <- gets maxdepth
  when (d>m) $ modify $ putmaxdepth d

