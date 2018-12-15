module BaseParser where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)] )

parse :: Parser a -> String -> [(a,String)]
parse (P p) string = p string

item :: Parser Char
item = P ( \string -> 
            case string of 
            [] -> []       
            (x:xs) -> [(x,xs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P ( \inp -> case parse p inp of 
                            [] -> []
                            [(v,out)] -> [(g v,out)])

instance Applicative Parser where
    pure v = P (\string -> [(v,string)])

    pg <*> px = P ( \string -> case parse pg string of 
        [] -> []
        [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P ( \inp -> case parse p inp of 
                            [] -> []
                            [(v,out)] -> parse (f v) out )
                            
instance Alternative Parser where
    -- empty :: Parser a
    empty = P ( \inp -> [])

    -- p <|> q :: Parser a -> Parser a -> Parser a
    p <|> q = P  (\inp -> case parse p inp of
                            [] -> parse q inp
                            [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
-- sat p = do x <- item
--            if p x then return x else empty    
sat p = item >>= \x ->
        if p x
            then return x
            else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x 
                   string xs 
                   return (x:xs) -- parser that applies id on the out and has the string on the head

ident :: Parser String
ident = do x <- lower 
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat 
         return (-n)
      <|> nat 
    
token :: Parser a -> Parser a
token p = do space
             v <- p
             space 
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

openPar :: Parser String
openPar = symbol "("

closedPar :: Parser String
closedPar = symbol ")"

semicolonList :: Parser a -> Parser [a]
semicolonList p = do 
        first <- p
        others <- many (do symbol ";"
                           p)
        return (first:others)