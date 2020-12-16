module PrintCaro where
import Prelude hiding((<>))
import RistrettoCaro
import Text.PrettyPrint

prVal :: Val -> Doc
prVal (VConst n) = int n
prVal (VParam n) = char 'a' <> int n
prVal (VLocal n) = char 'x' <> int n

-- >>> render $ prVal (VParam 7)
-- a7

-- | Pretty print an Exp
--
-- Examples:
--
-- >>> prExp $ ECall "print3" [x_ 1, c_ 1, c_ 0]
-- call print3(x1, 1, 0)
--
-- >>> prExp $  EOp OpAdd (c_ 1) (c_ 2)
-- add 1, 2
prExp :: Exp -> Doc
prExp (EVal v) = prVal v
prExp (ECall f vs) = text "call" <+>  text f <> parens (commasep (map prVal vs))
prExp (EOp op v1 v2) = prOp op <+> commasep [prVal v1, prVal v2]

commasep = hcat . punctuate (comma<>space)
prOp OpAdd = text "add"
prOp OpSub = text "sub"

testECall = ECall "print3" [x_ 1, c_ 1, c_ 0]
testEOp = EOp OpAdd (c_ 1) (c_ 2)

prInstr Inop = text "nop"
prInstr (Iret mv) = text "ret" <+> go mv where
    go Nothing = empty
    go (Just v) = prVal v
prInstr (Iexp e) = prExp e
prInstr (Iassign l e) = prVal (x_ l) <+> text "=" <+> prExp e

prInstrLst :: [Instr] -> Doc
prInstrLst = vcat . map prInstr

instance Show Instr where
    show = render . prInstr
    showList is s = render (prInstrLst is) ++ s
