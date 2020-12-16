module Main where

import System.Environment(getArgs, getProgName) 
import System.Exit(exitFailure )
import System.IO(stdin,withFile,Handle,hGetContents,hPutStrLn,IOMode(..))
import LexInstant
import ParInstant
import PrintInstant
import AbsInstant
import ErrM

import LlvmAsm
import LlvmState

genTree :: Program -> Handle -> IO()
genTree (Prog stmts) h = do
  let (addr, state) = runGen $ genStmts stmts
  let code = reverse $ rcode state
  let body = code
  mapM_ (hPutStrLn h) $  [
   "declare void @printInt(i32)",
   "define i32 @main() {",
   "entry: "]
   ++ body ++
   ["\tret i32 0"
   ,"}"
   ]

emitPrintInt :: Address -> GenM ()
emitPrintInt a = emit $ "\tcall void @printInt(i32 "++shows a ")"

genStmts :: [Stmt] -> GenM ()
genStmts ss = mapM_ genStmt ss

genStmt :: Stmt -> GenM ()
genStmt (SBlock ss) = genStmts ss
genStmt (SExp e) = do
  a <- genExp e
  emitPrintInt a
genStmt (SAss (Ident s) e) = do
  lhs <- getLocal s
  rhs <- genExp e
  emit $ store rhs lhs

genStmt (SIfElse c t e) = do
  lTrue <- freshLabel
  lFalse <- freshLabel
  lEnd <- freshLabel
  genCond c lTrue lFalse
  emit $ placeLabel lTrue
  genStmt t 
  emit $ goto lEnd
  emit $ placeLabel lFalse
  genStmt e
  emit $ goto lEnd
  emit $ placeLabel lEnd
  
genStmt (SWhile cond body) = do
  lCond <- freshLabel
  lBody <- freshLabel
  lEnd <- freshLabel
  emit $ goto lCond
  emit $ placeLabel lBody
  genStmt body
  emit $ goto lCond
  emit $ placeLabel lCond
  genCond cond lBody lEnd
  emit $ placeLabel lEnd

genCmp :: String -> Exp -> Exp -> Label -> Label -> GenM ()
genCmp comp e1 e2 lTrue lFalse = do
  t1 <- genExp e1
  t2 <- genExp e2
  c <- genTemp
  emit $ icmp comp c t1 t2
  emit $ branch c lTrue lFalse
  
genCond :: Cond -> Label -> Label -> GenM ()  
genCond (CEq e1 e2) lTrue lFalse = genCmp "eq" e1 e2 lTrue lFalse
genCond (CLt e1 e2) lTrue lFalse = genCmp "slt" e1 e2 lTrue lFalse
genCond (CGt e1 e2) lTrue lFalse = genCmp "sgt" e1 e2 lTrue lFalse
  
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

genExp :: Exp -> GenM Address
genExp (ExpInt i)  = return $ Immediate i
genExp (ExpNegInt i) = genExp (ExpInt (-i))
genExp (ExpAdd e1 e2) = genBinOp "add i32 " e1 e2
genExp (ExpSub e1 e2) = genBinOp "sub i32 " e1 e2
genExp (ExpMul e1 e2) = genBinOp "mul i32 " e1 e2
genExp (ExpDiv e1 e2) = genBinOp "sdiv i32 " e1 e2
genExp (ExpMod e1 e2) = genBinOp "srem i32 " e1 e2
genExp (ExpVar (Ident s)) = do
  a <- getLocal s 
  t <- genTemp
  emit $ load t a
  return t

genBinOp op e1 e2 = do
  a1 <- genExp e1
  a2 <- genExp e2
  t <- genTemp
  emit $ concat ["\t",show t,"=",op,show a1,",",show a2]
  return t

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
                          withFile "out.ll" WriteMode $ genTree tree


main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 2 pProgram
            "-s":fs -> mapM_ (runFile 0 pProgram) fs
            fs -> mapM_ (runFile 2 pProgram) fs





