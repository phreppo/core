module CoreParser where

import Language
import BaseParser
import Control.Applicative
import Data.Char

-- parseProg :: Parser (Program Name)
-- parseProg = do p <- parseScDef
--                do character ';'
--                   ps <- parseProg
--                   return (p:ps)
--                <|> return [p]

-- parseScDef :: Parser (ScDef Name)
-- parseScDef = do v <- identifier
--                 pf <- many identifier
--                 char '='
--                 body <- parseExpr -- call to parseExpr
--                 return (v, pf, body)

keywords = [
    "let",
    "letrec",
    "in",
    "case",
    "of",
    "Pack"]

parseExpr :: Parser CoreExpr
parseExpr =  parseLet
         <|> parseAExpr -- should go last
         <|> empty

--------------------------------------------------------------------------------
--                                    Let
--------------------------------------------------------------------------------

parseLet :: Parser CoreExpr
parseLet = do
    symbol "let"         
    definitions <- semicolonList parseDef
    symbol "in"
    e <- parseExpr
    return $ ELet nonRecursive definitions e

parseDef :: Parser CoreDef
parseDef = do
    (EVar var) <- parseEVar
    symbol "="
    exp <- parseExpr
    return (var, exp)

--------------------------------------------------------------------------------
--                                  AEXpr
--------------------------------------------------------------------------------

parseAExpr :: Parser CoreExpr
parseAExpr =  parseEVar
          <|> parseENum
          <|> parseConstructor
          <|> parseAExprPar

parseEVar :: Parser CoreExpr
parseEVar = do
    var <- identifier
    if elem var keywords
        -- variables should not be keywords
        then empty
        else return $ EVar var

parseENum :: Parser CoreExpr
parseENum = do
    n <- integer 
    return $ ENum n

parseConstructor :: Parser CoreExpr
parseConstructor = do
    symbol "Pack{"
    n1 <- integer
    symbol ","
    n2 <- integer
    symbol "}"
    return $ EConstr n1 n2

parseAExprPar :: Parser CoreExpr
parseAExprPar = do 
    openPar
    e <- parseExpr
    closedPar
    return e

-- parseAlt :: Parser CoreAlt
-- parseAlt = \s -> []