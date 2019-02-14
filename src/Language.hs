module Language
where

type Program a = [ScDef a]
type CoreProgram = Program Name

type ScDef a = (Name, [a], Expr a)
type CoreScDef = ScDef Name

data Expr a
    = EVar Name
    | ENum Int
    | EConstr Int Int
    | EAp (Expr a) (Expr a)
    | ELet IsRec [Def a] (Expr a)
    | ECase (Expr a) [Alter a]
    | ELam [a] (Expr a)
    deriving (Show)
type CoreExpr = Expr Name

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

type Def a = (a, Expr a)
type CoreDef = Def Name

type Name  = String
data IsRec = NonRecursive | Recursive
           deriving Show

keywords :: [String]
keywords = [
    "let",
    "letrec",
    "in",
    "case",
    "of",
    "Pack"]

relops :: [String]
relops = [
    "<=",
    "<", 
    "==",
    "~=",
    ">=",
    ">"
    ]

--------------------------------------------------------------------------------
--                              Utility functions
--------------------------------------------------------------------------------

recursive :: IsRec
recursive    = Recursive

nonRecursive :: IsRec
nonRecursive = NonRecursive

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr e = False

bindersOf :: [(a,b)] -> [a] -- for let
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

--------------------------------------------------------------------------------
--                              Prelude Definitions
--------------------------------------------------------------------------------

preludeDefs :: CoreProgram
preludeDefs = [
    ("I", ["x"], EVar "x"),
    ("K", ["x","y"], EVar "x"),
    ("K1",["x","y"], EVar "y"),
    ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                             (EAp (EVar "g") (EVar "x"))),
    ("compose", ["f","g","x"], EAp (EVar "f")
                                   (EAp (EVar "g") (EVar "x"))),
    ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
  ]