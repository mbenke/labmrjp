module Main where
import Asm
import RistrettoArgo
import TranslateArgo

print3 = FunDef "print3" 0 body where
  body = [ Icall "printInt" [a 1]
         , Icall "printInt" [a 2]
         , Icall "printInt" [a 3]
         , Iretvoid
         ] where a = VParam

mymain = FunDef { fun_name = "mymain", fun_locals = 1, fun_body = body } where
    body = [ Icall  "printInt" [c 3]
           , Istore 1 (VConst 42) 
           , Icall  "printInt3" [c 2, c 1, c 0]
           , Icall  "print3" [c 2, c 1, c 0]
           , Iretint (x 1)
           ] where [c, x] = [VConst, VLocal]

main = mapM_  (putStr . xlatFun) [mymain,print3]
