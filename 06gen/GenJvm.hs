module Main where

import System.Environment(getArgs, getProgName) 
import System.Exit(exitFailure )
import System.IO(stdin,withFile,Handle,hGetContents,hPutStrLn,IOMode(..))
import Control.Monad

import LexInstant
import ParInstant
import PrintInstant
import AbsInstant
import ErrM

import qualified ISyntax as I
import JvmState
import Jasmin
import OptJvm

type Code = [JInstr]

genTree :: Program -> Handle -> IO()
genTree (Prog stmts) h = do
  let (a,state) = runGen $ genStmts stmts
  let body = reverse $ rcode state
  let optbody = optimise body
  let stacklimit = maxdepth state
  let loclimit = nextlocal state
  hPutStrLn h $ showClass [
      mainMethod stacklimit loclimit (CodeSyms $ optbody ++ [Jreturn]),
      printIntMethod                                                        
    ]

         
printIntCode :: GenM Code -> GenM Code
printIntCode codegen = do
  code <- codegen
  return $ code ++ [Jinvoke "Instant" [Tint] Tvoid "printInt"]

genStmts :: [Stmt] -> GenM ()
genStmts ss = mapM_ genStmt ss


genStmt :: Stmt -> GenM ()
genStmt (SBlock ss) = genStmts ss
genStmt (SExp e) = do
  genExp e
  emit $ Jinvoke "Instant" [Tint] Tvoid "printInt"
  
genStmt (SAss (Ident s) e) = do
  lhs <- getLocal s
  genExp e
  emit $ JIstore lhs
  
genStmt (SIfElse c t e) = do
  lThen <- freshLabel
  lElse <- freshLabel
  lEnd <- freshLabel
  genCond2 c lThen lElse lThen
  emit $ Jlabel lThen
  genStmt t 
  emit $ Jgoto lEnd
  emit $ Jlabel lElse
  genStmt e
  emit $ Jlabel lEnd
  
genStmt (SWhile cond body) = do
  lCond <- freshLabel
  lBody <- freshLabel
  lEnd <- freshLabel
  emit $ goto lCond
  emit $ placeLabel lBody
  genStmt body
  emit $ placeLabel lCond
  genCond cond lBody lEnd
  emit $ placeLabel lEnd
  
genCond :: Cond -> Label -> Label -> GenM ()  
genCond (CEq e1 (ExpInt 0)) lThen lElse = do
  genExp e1
  emit $ Jifeq lThen
  emit $ Jgoto lElse
genCond (CEq e1 e2) lThen lElse = do
  genExp e1
  genExp e2
  emit $ Jif_icmpeq lThen
  emit $ Jgoto lElse
genCond (CLt e1 (ExpInt 0)) lThen lElse = do
  genExp e1
  emit $ Jiflt lThen
  emit $ Jgoto lElse
genCond e@(CLt e1 e2) lThen lElse = do
  genExp e1
  genExp e2
  emit $ Jif_icmplt lThen
  emit $ Jgoto lElse
genCond (CGt e1 (ExpInt 0)) lThen lElse = do
  genExp e1
  emit $ Jifgt lThen
  emit $ Jgoto lElse
genCond (CGt e1 e2) lThen lElse = do
  genExp e1
  genExp e2
  emit $ Jif_icmpgt lThen
  emit $ Jgoto lElse
  
genCond (CAnd c1 c2) lTrue lFalse = do
  lMid <- freshLabel
  genCond c1 lMid lFalse
  emit $ placeLabel lMid
  genCond c2 lTrue lFalse
  
genCond (COr c1 c2) lTrue lFalse = do
  lMid <- freshLabel
  genCond c1 lTrue lMid
  emit $ placeLabel lMid
  genCond c2 lTrue lFalse

genCond (CNot c) lTrue lFalse = genCond c lFalse lTrue

-- |genCond2 condition lTrue lFalse lNext
genCond2 :: Cond -> Label -> Label -> Label -> GenM ()  
genCond2 (CEq e1 (ExpInt 0)) lThen lElse lNext = do
  genExp e1
  emit $ Jifeq lThen
  emit $ Jgoto lElse
genCond2 (CEq e1 e2) lThen lElse lNext = do
  genExp e1
  genExp e2
  emit $ Jif_icmpeq lThen
  emit $ Jgoto lElse
genCond2 (CLt e1 (ExpInt 0)) lThen lElse lNext = do
  genExp e1
  genCondJumps lThen lElse lNext Jiflt Jifge
genCond2 e@(CLt e1 e2) lThen lElse lNext = do
  genExp e1
  genExp e2
  emit $ Jif_icmplt lThen
  emit $ Jgoto lElse
genCond2 (CGt e1 (ExpInt 0)) lThen lElse lNext = do
  genExp e1
  genCondJumps lThen lElse lNext Jifgt Jifle
genCond2 (CGt e1 e2) lThen lElse lNext = do
  genExp e1
  genExp e2
  genCondJumps lThen lElse lNext Jif_icmpgt Jif_icmple 
  
genCond2 (CAnd c1 c2) lTrue lFalse lNext = do
  lMid <- freshLabel
  genCond2 c1 lMid lFalse lMid
  emit $ placeLabel lMid 
  genCond2 c2 lTrue lFalse lNext
  
genCond2 (COr c1 c2) lTrue lFalse lNext = do
  lMid <- freshLabel
  genCond2 c1 lTrue lMid lMid
  emit $ placeLabel lMid 
  genCond2 c2 lTrue lFalse lNext

genCond2 (CNot c) lTrue lFalse lNext = genCond2 c lFalse lTrue lNext

genCondJumps lThen lElse lNext posJump negJump
      | lNext == lThen = do
        emit $ negJump lElse
      | lNext == lElse = do
        emit $ posJump lThen 
      | otherwise = do
        emit $ posJump lThen 
        emit $ Jgoto lElse

genExp :: Exp -> GenM ()
genExp (ExpInt i) = incStack 1 >> emit (JIconst i)
genExp (ExpNegInt i) = genExp (ExpInt (-i))
genExp (ExpAdd e1 e2) = genBinOp JTadd e1 e2
genExp (ExpSub e1 e2) = genBinOp JTsub e1 e2
genExp (ExpMul e1 e2) = genBinOp JTmul e1 e2
genExp (ExpDiv e1 e2) = genBinOp JTdiv e1 e2
genExp (ExpMod e1 e2) = genBinOp JTmod e1 e2
genExp (ExpVar (Ident s)) = do
  n <- getLocal s 
  emit $ JIload n
  
genBinOp op e1 e2 = do
   genExp e1
   genExp e2
   incStack (-1)
   emit $ JOper op Tint

------------------------------------------------------------------

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          withFile "out.j" WriteMode $ genTree tree


main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 2 pProgram
            [f] -> runFile 2 pProgram f
