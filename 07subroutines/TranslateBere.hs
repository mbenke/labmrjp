module TranslateBere(xlatFun) where

import Asm
import RistrettoBere

xlatFun :: FunDef -> AsmText
xlatFun f =  funText (fun_name f) (xlatBody (fun_body f))
xlatBody :: Instrs -> FunName -> AsmText
xlatBody is fn = concat (map (xlatInstr  fn) is)
xlatInstr :: FunName -> Instr -> AsmText
xlatInstr fn (Iretint v) = unlines [setResult v, "leave","ret"]
xlatInstr fn (Iretvoid) = unlines ["leave","ret"]
xlatInstr fn (Istore n v) = unlines [lda $ xlatVal v, sta $ xlatVal (VLocal n)]
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
xlatInstr fn (Iadd resloc v1 v2) = unlines 
 [ ldaVal v1
 , lddVal v2
 , "add %edx, %eax"
 , staLoc resloc
 ]

ldaVal = lda . xlatVal
lddVal = ldd . xlatVal
staLoc n = sta $ xlatVal (VLocal n)
   
pushVal v = "pushl "++ opText (xlatVal v)

xlatVal :: Val -> Operand
xlatVal (VConst n) = OpImm n
xlatVal (VParam n) = OpFpRel (4*n+4)
xlatVal (VLocal n) = OpFpRel (-4*n)

fpRelative n = show n ++ "(%ebp)"
setResult = lda . xlatVal
