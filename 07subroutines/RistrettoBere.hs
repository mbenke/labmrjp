module RistrettoBere where
type FunName = String

data Instr
  = Iretvoid
  | Iretint Val         -- return i
  | Icall FunName [Val]
  | Istore Int Val      -- local_i := v 
  | Iadd Int Val Val    -- local_i := v1 + v2
  | Isub Int Val Val    -- local_i := v1 - v2

instance Show Instr where
  showList is s = unlines (map show is) ++ s
  showsPrec p (Iretvoid)  = showString "ret"
  showsPrec p (Iretint n) = showString "ret " . shows n
  showsPrec p (Icall f as) = showString "call " . showString f 
                           . showParen True (showList as)
  showsPrec p (Istore n v) = showChar 'x' . shows n . showString " = " . shows v
  showsPrec p (Iadd n v1 v2) = showsQuad '+' n v1 v2
  showsPrec p (Isub n v1 v2) = showsQuad '-' n v1 v2


showsQuad op n v1 v2 = showChar 'x' . shows n . showString " = "
                   . shows v1 . showChar op . shows v2
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
