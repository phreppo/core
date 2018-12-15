module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)] )

parse :: Parser a -> String -> [(a,String)]
parse (P p) string = p string

item :: Parser Char
item = P ( \inp -> 
            case inp of 
            [] -> []       
            (x:xs) -> [(x,xs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P ( \inp -> case parse p inp of 
                            [] -> []
                            [(v,out)] -> [(g v,out)])

instance Applicative Parser where
    pure v = P (\imp -> [(v,imp)])

    pg <*> px = P ( \imp -> case parse pg imp of 
        [] -> []
        [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P ( \inp -> case parse p inp of 
                            [] -> []
                            [(v,out)] -> parse (f v) out
                )
                            
instance Alternative Parser where
    empty = P ( \inp -> [])
    p <|> q = P  (\inp -> case parse p inp of
                            [] -> parse q inp
                            [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty                             

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
                   return (x:xs)

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
identifier = token identifier

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)