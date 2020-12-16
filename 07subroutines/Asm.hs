{-# LANGUAGE CPP #-}
module Asm where
type AsmText = String

#ifdef darwin_HOST_OS
mangle name = '_':name
#else
mangle name = name
#endif

data Operand = OpImm Int | OpFpRel Int
opText :: Operand -> AsmText
opText (OpImm n) = '$':show n
opText (OpFpRel d) = show d ++ "(%ebp)"

funStart :: String -> AsmText
funStart name = unlines [".globl "++mangle name,mangle name++":"]
funProlog locals = unlines ["push %ebp","mov %esp, %ebp", subsp size]
  where size = padTo16 (4*locals+8) - 8 -- 8 bytes for ret+DL
        padTo16 n = 16+16*(n `div` 16)


funText :: String -> (String -> AsmText) -> AsmText 
funText name funBody = unlines frags where
 frags  :: [String]
 frags =
  [ funStart name
  , funProlog 0
  , funBody name
  , unlines [labelEnd name++":","leave",ret]
  ]

labelEnd name = name++".end"

lda :: Operand -> AsmText
lda op = concat ["mov ",opText op, ", %eax"]

sta :: Operand -> AsmText
sta op = concat ["mov ", "%eax, ",opText op]

ldd :: Operand -> AsmText
ldd op = concat ["mov ",opText op, ", %edx"]

std :: Operand -> AsmText
std op = concat ["mov ", "%edx, ",opText op]

subsp :: Int -> String
subsp n = concat ["sub ",'$':show n,", ", "%esp"]

addsp :: Int -> String
addsp n = concat ["add ",'$':show n,", ", "%esp"]

ret = "ret"

iret :: String -> Operand -> AsmText
iret funName n = unlines [lda n, jmp $ labelEnd funName]
jmp label = "jmp "++label

call funName = "call " ++ mangle funName
