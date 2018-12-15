module Language
where

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

data Expr a
    = EVar Name
    | ENum Int
    | EConstr Int Int
    | EAp (Expr a) (Expr a)
    | ELet IsRec [(a, Expr a)] (Expr a)
    | ECase (Expr a) [Alter a]
    | ELam [a] (Expr a)
    deriving (Show)
type CoreExpr = Expr Name

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

type Name  = String
type IsRec = Bool

--------------------------------------------------------------------------------
--                              Utility functions
--------------------------------------------------------------------------------

recursive :: IsRec
recursive    = True

nonRecursive :: IsRec
nonRecursive = False

isAtomicExpr :: Expr a -> Bool
isAtomicExpr(EVar v) = True
isAtomicExpr(ENum n) = True
isAtomicExpr e = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]