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

-- parseExpr :: Parser (Expr Name)
-- parseExpr = do x <- parseAExpr
--                return x

parseAExpr :: Parser (Expr Name)
parseAExpr =  parseEVar
          <|> parseENum
          <|> parseConstructor
          <|> parseAExprPar

parseEVar :: Parser (Expr Name)
parseEVar = do
    var <- identifier
    return $ EVar var

parseENum :: Parser (Expr Name)
parseENum = do
    n <- integer 
    return $ ENum n

parseConstructor :: Parser (Expr Name)
parseConstructor = do
    symbol "Pack{"
    n1 <- integer
    symbol ","
    n2 <- integer
    symbol "}"
    return $ EConstr n1 n2

parseAExprPar :: Parser (Expr Name)
parseAExprPar = do 
    openPar
    -- TODO: once parseExpr implemented, put it here
    closedPar
    return $ ENum 666

-- parseDef :: Parser (Def Name)
-- parseDef = \s -> []
-- parseAlt :: Parser (Alter Name)
-- parseAlt = \s -> []