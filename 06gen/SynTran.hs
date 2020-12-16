module SynTran where
import qualified AbsInstant as A
import ISyntax

transProg (A.Prog ss) = map transStmt ss

transExp :: A.Exp -> Exp String
transExp (A.ExpAdd e1 e2) = ExpBin BAdd (transExp e1) (transExp e2)
transExp (A.ExpSub e1 e2) = ExpBin BSub (transExp e1) (transExp e2)
transExp (A.ExpMul e1 e2) = ExpBin BMul (transExp e1) (transExp e2)
transExp (A.ExpDiv e1 e2) = ExpBin BDiv (transExp e1) (transExp e2)
transExp (A.ExpMod e1 e2) = ExpBin BMod (transExp e1) (transExp e2)
transExp (A.ExpInt i) = ExpInt i
transExp (A.ExpNegInt i) = ExpInt (-i)
transExp (A.ExpVar (A.Ident s)) = ExpVar s

transStmt (A.SAss (A.Ident s) e) = SAss s $ transExp e
transStmt (A.SExp e) = SExp $ transExp e

