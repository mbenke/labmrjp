{-# LANGUAGE CPP #-}
module Main where
import RistrettoBere
import TranslateBere

printInt v = Icall "printInt" [v]
print3 = FunDef "print3" 0 body where
  body = [ printInt $ VParam 1
         , printInt $ VParam 2
         , printInt $ VParam 3
         , Iretvoid
         ]

mymain = FunDef { fun_name = "mymain", fun_locals = 1, fun_body = body } where
    body = [ Icall  "printInt" [VConst 3]
           , Iadd 1 (VConst 40) (VConst 2) 
           , printInt $ (VLocal 1)
           , Icall  "printInt3" [VConst 2, VConst 1, VConst 0]
           , Icall  "print3" [VConst 2, VConst 1, VConst 0]
           , Iretint (VLocal 1)
           ]

main = mapM_  (putStr . xlatFun) [mymain,print3]
