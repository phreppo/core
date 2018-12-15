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

parseExpr :: Parser CoreExpr
parseExpr = parseAExpr

parseAExpr :: Parser CoreExpr
parseAExpr =  parseEVar
          <|> parseENum
          <|> parseConstructor
          <|> parseAExprPar

parseEVar :: Parser CoreExpr
parseEVar = do
    var <- identifier
    return $ EVar var

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

-- parseDef :: Parser (Def Name)
-- parseDef = \s -> []
-- parseAlt :: Parser (Alter Name)
-- parseAlt = \s -> []