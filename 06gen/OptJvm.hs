module OptJvm where
import Jasmin

optimise code = peepStep code

peepStep :: [JInstr] -> [JInstr]
peepStep [] = []
--  "store i; load i" -> "dup; store i"
peepStep (JIstore i:JIload j:code)
  | i == j = Jdup:JIstore i:peepStep code
  | otherwise = JIstore i:JIload j:peepStep code
peepStep (JIreturn:Jgoto l:code) = JIreturn:peepStep code
peepStep (i:c) = i:peepStep c
-- peepStep x = error $ "peepStep: " ++ show x
