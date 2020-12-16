{-# LANGUAGE CPP #-}
module Main where
import Asm

type FunName = String
funBody :: FunName -> AsmText
funBody funName = unlines [iret funName (OpImm 42)]

main = putStr (funText "mymain" funBody)
