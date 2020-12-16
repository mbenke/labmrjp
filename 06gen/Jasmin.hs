module Jasmin where

data Method = Method { 
  mattrs :: String,
  mname :: String,
  mrtype :: Type,
  margs :: [Type],
  mdepth :: Int, 
  mlocals :: Int, 
  mcode :: JavaCode 
  }
                 
data JavaCode = 
      CodeLines [String]
    | CodeString String  
    | CodeSyms [JInstr]  

type Label = String

showCode :: JavaCode -> String
showCode (CodeLines ls) = unlines ls
showCode (CodeString s) = s
showCode (CodeSyms is) = unlines $ map printC is

showMethod :: Method -> String
showMethod m = unlines $ [
 unwords [".method", mattrs m, showMethodSig m],
 ".limit stack " ++ show (mdepth m),
 ".limit locals " ++ show (mlocals m),
 showCode $ mcode m,
 ".end method"
 ]
 
showMethodSig :: Method -> String
showMethodSig (Method attrs name ret args d l c) = showSig name ret args

showSig name ret args = name ++ "(" ++ concatMap show args ++ ")" ++ show ret

showClass :: [Method] -> String
showClass ms = "\
\.class  public Instant\n\
\.super  java/lang/Object\n\
\\n\
\;\n\
\; standard initializer\n\
\.method public <init>()V\n\
\   aload_0\n\
\   invokespecial java/lang/Object/<init>()V\n\
\   return\n\
\.end method\n" ++ unlines (map showMethod ms)


data JInstr = JIconst Integer |
              JDconst Double |
              JIload Int |
              JDload Int |
              JOper Oper Type |
              JIstore Int | 
              JDstore Int |
              JIinc Int Integer |
              JDinc Int Double |
              JIreturn |
              JDreturn |
              Jreturn |
              Jifeq String | 
              Jifne String |
              Jifgt String |
              Jiflt String |
              Jifge String |
              Jifle String |
              Jif_icmpeq String |
              Jif_icmpge String |
              Jif_icmple String |
              Jif_icmpne String |
              Jif_icmpgt String |
              Jif_icmplt String |
              JDcmpg | JDcmpl | JIneg | JDneg |
              Jgoto String | Jlabel String |
              Jinvoke String [Type] Type String |
              JIO String |
              Jldc String |
              JI2D |
              JD2I |
              Jdup

data Oper = JTadd | JTsub | JTmul | JTdiv | JTmod 
data Type = Tint | Tdouble | Tbool | Tvoid | Tspec String deriving Eq

instance Show Type where
  show (Tint) = "I"
  show (Tdouble) = "D"
  show (Tbool) = "I"
  show (Tvoid) = "V"
  show (Tspec s) = s


-- data EType = Var Type

printC (JIconst x)
 | x == (-1) = "iconst_m1"
 | (0 <= x) && (x < 5) = "iconst_" ++ show x
 | (abs x) < 2^7 = "bipush " ++ show x
 | (abs x) < 2^15 = "sipush " ++ show x
 | otherwise = "ldc " ++ show x
printC (JDconst x)
 | (x == 0) || (x == 1) = "dconst_" ++ show (round x)
 | otherwise = "ldc2_w " ++ show x
printC (JIload x)
 | (0 <= x) && (x < 4) = "iload_" ++ show x
 | x < 2^8 = "iload " ++ show x
 | otherwise = "wide iload " ++ show x
printC (JDload x)
 | (0 <= 0) && (x < 4) = "dload_" ++ show x
 | x < 2^8 = "dload " ++ show x
 | otherwise = "wide dload " ++ show x
printC (JOper JTadd Tint) = "iadd"
printC (JOper JTadd Tdouble) = "dadd"
printC (JOper JTsub Tint) = "isub"
printC (JOper JTsub Tdouble) = "dsub"
printC (JOper JTmul Tint) = "imul"
printC (JOper JTmul Tdouble) = "dmul"
printC (JOper JTdiv Tint) = "idiv"
printC (JOper JTdiv Tdouble) = "ddiv"
printC (JOper JTmod Tint) = "irem"
printC (JOper JTmod Tdouble) = "drem"
printC (JIstore x)
 | (0 <= x) && (x < 4) = "istore_" ++ show x
 | x < 2^8 = "istore " ++ show x
 | otherwise = "wide istor " ++ show x
printC (JDstore x)
 | (0 <= x) && (x < 4) = "dstore_" ++ show x
 | x < 2^8 = "dstore " ++ show x
 | otherwise = "wide dstore " ++ show x
printC (JIinc x n)
 | x < 2^8 = "iinc " ++ show x ++ " " ++ show n
 | otherwise = "wide iinc " ++ show x ++ " " ++ show n
printC (JDinc x n)
 | x < 2^8 = "dload " ++ show x ++ "dconst_1" ++ "dadd"
 | otherwise = "wide dload " ++ show x ++ "dconst_1" ++ "dadd"
printC (JIreturn) = "ireturn"
printC (JDreturn) = "dreturn"
printC (Jreturn) = "return"
printC (Jifeq x) = "ifeq " ++ x
printC (Jifne x) = "ifne " ++ x
printC (Jifgt x) = "ifgt " ++ x
printC (Jiflt x) = "iflt " ++ x
printC (Jifge x) = "ifge " ++ x
printC (Jifle x) = "ifle " ++ x
printC (Jif_icmpeq x) = "if_icmpeq " ++ x
printC (Jif_icmpge x) = "if_icmpge " ++ x
printC (Jif_icmple x) = "if_icmple " ++ x
printC (Jif_icmpne x) = "if_icmpne " ++ x
printC (Jif_icmpgt x) = "if_icmpgt " ++ x
printC (Jif_icmplt x) = "if_icmplt " ++ x
printC (JDcmpg) = "dcmpg"
printC (JDcmpl) = "dcmpl"
printC (JIneg) = "ineg"
printC (JDneg) = "dneg"
printC (Jgoto x) = "goto " ++ x
printC (Jlabel x) = x ++ ":"
printC (JIO x) = "invokestatic " ++ x
printC (Jinvoke cl typs typ x) = "invokestatic " ++ cl ++ "/" ++ x ++
                                  "(" ++ concatMap show typs ++ ")" ++
                                    show typ
printC (Jldc x) = "ldc " ++ show x
printC (JI2D) = "i2d"
printC (JD2I) = "d2i"
printC (Jdup) = "dup"

instance Show JInstr where
  show = printC
  showList is s = unlines (map show is) ++ s -- FIXME
  
mainMethod :: Int -> Int -> JavaCode -> Method
mainMethod d l code = Method "public static" "main" Tvoid args d l code where
  args = [Tspec "[Ljava/lang/String;"]
  
printIntMethod :: Method 
printIntMethod = Method "static" "printInt" Tvoid [Tint] 2 1 code where
  code = CodeLines [
    "getstatic java/lang/System/out Ljava/io/PrintStream;",
    "iload_0",
    "invokevirtual java/io/PrintStream/println(I)V",
    "return"
    ]

-- For JVM <-> LLVM portability
placeLabel :: Label -> JInstr
placeLabel l = Jlabel l

goto :: Label -> JInstr
goto = Jgoto