module RistrettoArgo where
type FunName = String

data Instr
  = Iretvoid
  | Iretint Val         -- return i
  | Icall FunName [Val]
  | Istore Int Val      -- local_i := v 

instance Show Instr where
  showList is s = unlines (map show is) ++ s
  showsPrec p (Iretvoid)   = showString "ret"
  showsPrec p (Iretint n)  = showString "ret " . shows n
  showsPrec p (Icall f as) = showString "call " . showString f 
                           . showParen True (showList as)
  showsPrec p (Istore n v) = showChar 'x' . shows n . showString " = " . shows v

data Val = VConst Int | VParam Int | VLocal Int

instance Show Val where
  show (VConst n) = show n
  show (VParam n) = 'a':show n
  show (VLocal n) = 'x':show n
   
{-
-- to use less VConst, one may try
instance Num Val where
    fromInteger = VConst . fromInteger
    (+) = undefined
    (*) = undefined
    (-) = undefined
    abs = undefined
    signum = undefined
-}

type Instrs = [Instr]

data FunDef = FunDef { fun_name :: FunName
                     , fun_locals :: Int
                     , fun_body :: Instrs 
                     } 

instance Show FunDef where
  showsPrec _ fd = showString "def " . showString name . showString ":\n" 
                   . showsLocals . showList (fun_body fd) where
    showsLocals | locals > 0 = showString "locals " . shows locals . showChar '\n'
                | otherwise = id
    locals = fun_locals fd
    name = fun_name fd

                  
sampleFun1 = FunDef "print3" 0 body where
  body = [ Icall "printInt" [a 1]
         , Icall "printInt" [a 2]
         , Icall "printInt" [a 3]
         , Iretvoid
         ] where a = VParam

sampleFun2 = FunDef { fun_name = "mymain", fun_locals = 0, fun_body = body } where
    body = [ Icall "printInt" [c 3]
           , Icall "printInt3" [c 2, c 1, c 0]
           , Icall "print3" [c 2, c 1, c 0]
           , Iretint (c 42)
           ] where [c,a,x] = [VConst,VParam,VLocal]
  
sampleFun3 = FunDef { fun_name = "mymain", fun_locals = 1, fun_body = body } where
    body = [ Icall  "printInt" [c 3]
           , Istore 1 (c 42) 
           , Icall  "print3" [x 1, c 1, c 0]
           , Iretint (x 1)
           ] where [c,a,x] = [VConst,VParam,VLocal]

