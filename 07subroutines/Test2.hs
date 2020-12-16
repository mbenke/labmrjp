{-# LANGUAGE CPP #-}
module Main where
import Asm

type FunName = String
data Instr
  = Ilda Int
  | Iretvoid
  | Iretint Int         -- return i
  | Icall FunName [Val]
  | Istore Int Val      -- local_i := v 

data Val = VConst Int | VParam Int | VLocal Int

type Instrs = [Instr]

data FunDef = FunDef { fun_name :: FunName
                     , fun_locals :: Int
                     , fun_body :: Instrs 
                     } 


print3 = FunDef "print3" 0 body where
  body = [ Icall "printInt" [VParam 1]
         , Icall "printInt" [VParam 2]
         , Icall "printInt" [VParam 3]
         , Iretvoid
         ]

mymain = FunDef { fun_name = "mymain", fun_locals = 0, fun_body = body } where
    body = [ Icall "printInt" [VConst 3]
           , Icall "printInt3" [VConst 2, VConst 1, VConst 0]
           , Icall "print3" [VConst 2, VConst 1, VConst 0]
           , Iretint 42
           ]

main = mapM_  (putStr . xlatFun) [mymain,print3]

xlatFun :: FunDef -> AsmText
xlatFun f =  funText (fun_name f) (xlatBody (fun_body f))
xlatBody :: Instrs -> FunName -> AsmText
xlatBody is fn = concat (map (xlatInstr  fn) is)
xlatInstr :: FunName -> Instr -> AsmText
xlatInstr fn (Iretint n) = iret fn (OpImm n)
xlatInstr fn (Iretvoid) = unlines ["leave","ret"]
xlatInstr fn (Icall callee args) = unlines
  [ subsp padding,
    pushargs,
    call callee,
    addsp (argsize+padding)
  ] 
  where
    argsize = 4*length args
    padding 
      | argsize == 0 = 0
      | otherwise    = 16 - argsize `mod` 16
    pushargs = unlines (map pushVal $ reverse args)

pushVal v = "pushl "++ xlatVal v

xlatVal (VConst n) = '$':show n
xlatVal (VParam n) = fpRelative (4*n+4)
xlatVal (VLocal n) = fpRelative (-4*n)

fpRelative n = show n ++ "(%ebp)"
