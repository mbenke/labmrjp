module RistrettoCaro where

type FunName = String
type Loc = Int

data Instr
  = Iret (Maybe Val)
  | Iassign Loc Exp
  | Iexp Exp
  | Inop
  -- deriving Show

data Exp = EOp Op Val Val
         | ECall String [Val]
         | EVal Val -- this should be eliminated
         deriving Show
data Op = OpAdd | OpSub
instance Show Op where
    show OpAdd = "add"
    show OpSub = "sub"

data Val = VConst Int | VParam Int | VLocal Int deriving Eq
c_ = VConst
x_ = VLocal
a_ = VParam

instance Show Val where
  show (VConst n) = show n
  show (VParam n) = 'a':show n
  show (VLocal n) = 'x':show n


type Instrs = [Instr]

data FunDef = FunDef { fun_name :: FunName
                     , fun_locals :: Int
                     , fun_body :: Instrs
                     }
