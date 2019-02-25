module PrettyPrinter where

import Language

-- ==================== Type Infrastructure ====================

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline


iNil :: Iseq -- The empty iseq
iNil = INil

iStr :: String -> Iseq -- Turn a string into an iseq
iStr str = IStr str

iAppend :: Iseq -> Iseq -> Iseq -- Append two iseqs
iAppend seq1 seq2 = IAppend seq1 seq2

iNewline :: Iseq -- New line with indentation
iNewline =  INewline

iIndent :: Iseq -> Iseq -- Indent an iseq
iIndent seq = IIndent seq

iDisplay :: Iseq -> String -- Turn an iseq into a string
iDisplay seq = flatten 0 [(seq,0)]

-- key
flatten :: Int -- Current column; 0 for first column
           -> [(Iseq, Int)] -- Work list
           -> String -- Result

flatten col [] = ""
flatten col ((INil,   indent)              : seqs) = flatten col seqs
flatten col ((IStr s, indent)              : seqs) = s ++ (flatten col seqs)
flatten col (((IAppend seq1 seq2), indent) : seqs) = 
    flatten col ((seq1, indent) : (seq2,indent) : seqs)
flatten col ((INewline, indent) : seqs) = 
-- questo replicate non so se sia giusto
    '\n' : (replicate indent ' ') ++ (flatten indent seqs) 
flatten col ((IIndent seq, indent) : seqs) = 
    flatten col ((seq, col) : seqs)
 
 


-- ==================== ISeq utility ====================

iConcat :: [Iseq] -> Iseq
iConcat [] = iNil
-- giusto?
iConcat (x:xs) = iAppend x (iConcat xs)
iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave int [] = iNil
-- giusto?
iInterleave int (x:xs) = iAppend x $ iAppend int $ iConcat xs

-- ==================== Pretty printer ====================

pprint :: CoreProgram -> IO ()
pprint prog = putStrLn $ iDisplay (pprProgram prog)

-- TODO: riscriverla di tipo CoreProgram -> Iseq
pprProgram :: CoreProgram -> Iseq
pprProgram [] = iNil
pprProgram ((n, vars, body):xs) = 
    (iStr n) `iAppend` (iStr " = ") `iAppend` (pprExpr body) 
    `iAppend` (iNewline)
    `iAppend` (pprProgram xs)

pprDefns :: [(Name,CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
 where sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr)
 = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
-- Inefficient version:  pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) = 
    iConcat 
    [ iStr keyword, iNewline,
      iStr " ", iIndent (pprDefns defns), iNewline,
      iStr "in ", pprExpr expr ]
    where keyword = if (isrec == NonRecursive) then "let" else "letrec"
-- TODO: case e lambda espressioni

pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = (iStr "(") `iAppend` pprExpr e `iAppend` (iStr ")")

-- ==================== Utility ====================
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
                   where e2s = e2 : e2s