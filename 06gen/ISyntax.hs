module ISyntax where

-- type Program i = [Stmt i]

data Stmt i 
  = SAss i (Exp i)
  | SExp (Exp i)  
    
data Exp i
    = ExpVar i
    | ExpInt Integer
    | ExpBin BinOp (Exp i) (Exp i)
      
data BinOp = BAdd | BSub | BMul | BDiv | BMod

                          