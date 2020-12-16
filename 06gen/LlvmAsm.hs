module LlvmAsm where

type Code = [LInstr]
type LInstr = String

data Address
  = Immediate Integer
  | Temp Int
  | Local Int

type Label = Int

instance Show Address where
    show (Immediate i) = show i
    show (Temp n) = "%i"++show n
    show (Local n) = "%loc"++show n

alloca :: Address -> LInstr
alloca a = concat ["\t", show a,"=alloca i32"]

store :: Address -> Address -> LInstr
store v a = concat ["\tstore i32 ",show v,", i32* ",show a]

load :: Address -> Address -> LInstr
load t a = concat ["\t",show t,"=","load i32, i32* ",show a]

placeLabel :: Label -> LInstr
placeLabel l = concat ["L",show l,":"]

useLabel :: Label -> String
useLabel l = concat ["label %L", show l]

goto :: Label -> LInstr
goto l = unwords ["\tbr", useLabel l]

branch :: Address -> Label -> Label -> LInstr
branch a t e = unwords ["\tbr i1 ",show a,",",useLabel t,",",useLabel e]

icmp :: String -> Address -> Address -> Address -> LInstr
icmp comp c t1 t2 = concat ["\t",show c,"=icmp ",comp," i32 ",show t1,",", show t2]
