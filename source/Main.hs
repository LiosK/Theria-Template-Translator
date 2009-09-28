module Main where

-- Copyright (c) 2009 LiosK (http://liosk.net/)
-- Licensed under The MIT License (http://liosk.net/-/license/mit)

import Char
import Text.ParserCombinators.Parsec

--------------------------------------------------------------------------------

main :: IO ()
main = do src <- getContents
          putStr $ compile src

run :: String -> IO ()
run src = putStrLn $ compile src

compile :: String -> String
compile src = case (runParser template defaultSettings "" src)
                of Left err -> show err
                   Right xs -> concat xs

--------------------------------------------------------------------------------
-- Config

-- MyParser type to deal with Config object
type MyParser = GenParser Char Config

data Config = Config { leftDelimiter        :: String,
                       rightDelimiter       :: String,
                       escapeShortTags      :: Bool,
                       tryTemplateTags      :: Bool }

-- Monadic config selector
conf selector = fmap selector getState

defaultSettings = Config { leftDelimiter        = "{",
                           rightDelimiter       = "}",
                           escapeShortTags      = True,
                           tryTemplateTags      = False }

--------------------------------------------------------------------------------

-- Top-level parser
template :: MyParser [String]
template = manyTill (tag <|> sot <|> fmap (:[]) anyChar) eof

-- Template tags (template command or output syntax enclosed by delimiters)
tag :: MyParser String
tag = do ld <- conf leftDelimiter
         string ld <?> "left delimiter"
         option ld (do xs <- command <|> output
                       rd <- conf rightDelimiter
                       string rd <?> "right delimiter"
                       return xs)

-- Short open tags
sot = do char '<'
         option "<" (do char '?'
                        option "<?php echo '<?'; ?>"
                               (do s <- try (string "php" >> space)
                                   return $ "<?php" ++ [s]))

-- Template commands (a command name and comma-separated arguments)
command = do char '#'
             nm <- name <?> "command name"
             args <- option [] (skipMany1 space >> sepBy1 expression sep)
             execute nm args

-- Output syntax (an expression and an optional output modifier)
output = do x <- expression
            y <- option Nothing (char ':' >> fmap Just lower)
            z <- modify y x
            return $ "<?php echo " ++ z ++ "; ?>"

--------------------------------------------------------------------------------
-- Expressions

data Expression = Expr { source, result :: String } deriving Show

-- Concatenates two Expressions
x `followedBy` y = Expr { source = source x ++ source y,
                          result = result x ++ result y }

-- Combinator to deal with optional following Expressions
x `canBeFollowedBy` y = option x (fmap (x `followedBy`) y)

-- Expression parser
expression :: MyParser Expression
expression = numberLiteral <|>
             stringLiteral <|>
             (char '!' >> function) <|>
             (char '$' >> variable) <|>
             member

function =
    do nm <- name <?> "function name"
       fmap (Expr { source = '!':nm, result = nm } `followedBy`) fnOperator

variable =
    do nm <- fmap ('$':) (name <?> "variable name")
       Expr { source = nm, result = nm } `canBeFollowedBy` operators

member =
    do nm <- name <?> "name label"
       Expr { source = nm,
              result = "$this->" ++ nm } `canBeFollowedBy` operators

--------------------------------------------------------------------------------
-- Operators

operators = fnOperator <|> arrOperator <|> objOperator

fnOperator =
    do args <- between (char '(') (char ')') (sepBy expression sep)
       Expr { source = "(" ++ join ", " (map source args) ++ ")",
              result = "(" ++ join ", " (map result args) ++ ")" } `canBeFollowedBy` objOperator

arrOperator =
    do xs <- between (char '[') (char ']') expression
       Expr { source = "[" ++ source xs ++ "]",
              result = "[" ++ result xs ++ "]" } `canBeFollowedBy` operators

objOperator =
    do d <- (char '.' >> option "" (string "$"))
       nm <- name <?> "name label"
       Expr { source = "." ++ d ++ nm,
              result = "->" ++ d ++ nm } `canBeFollowedBy` operators

--------------------------------------------------------------------------------
-- Basic Literals

-- Argument separator (comma)
sep = spaces >> char ',' >> spaces

-- Name label (/[_a-zA-Z][_a-zA-Z0-9]*/)
name = do x <- (char '_' <|> letter)
          fmap (x:) (many (char '_' <|> alphaNum))

-- String literal (/'(?:\\.|[^'])*'|"(?:\\.|[^"])*"/)
stringLiteral = do q <- oneOf "'\""
                   xs <- manyTill (stringChar q) (char q <?> "end of string")
                   return $ Expr { source = [q] ++ (concat xs) ++ [q],
                                   result = [q] ++ (concat xs) ++ [q] }

stringChar q = (char '\\' >> fmap (\c -> ['\\', c]) anyChar) <|> fmap (:[]) (noneOf [q])

-- Number literal
numberLiteral =
    do s <- sign
       n <- (do x <- try (char '0' >> oneOf "xX")
                fmap (['0', x] ++) (many1 hexDigit)
             <|>
             do ds <- many1 digit
                fs <- option "" (char '.' >> fmap ('.':) (many1 digit))
                es <- option "" (do e <- oneOf "eE"
                                    ss <- fmap (e:) sign
                                    fmap (ss ++) (many1 digit))
                return $ ds ++ fs ++ es)
       return $ Expr { source = s ++ n, result = s ++ n }

sign = option "" $ fmap (:[]) (oneOf "+-")

--------------------------------------------------------------------------------
-- Template command implementations

execute :: String -> [Expression] -> MyParser String

execute "comment" _ = return ""
execute "_" args = execute "comment" args

execute "sot" _ = return "<?"

execute "ldelim" _ = conf leftDelimiter
execute "rdelim" _ = conf rightDelimiter

execute "if" [] = return "<?php if (true): ?>"
execute "if" xs = return $ "<?php if (" ++ join " && " (map result xs) ++ "): ?>"
execute "elif" [] = return "<?php elseif (true): ?>"
execute "elif" xs = return $ "<?php elseif (" ++ join " && " (map result xs) ++ "): ?>"
execute "else" _ = return "<?php else: ?>"
execute "endif" _ = return "<?php endif; ?>"

execute "foreach" (x:xs) =
    return $ "<?php foreach (" ++ result x ++ " as "
             ++ (if (length xs > 1) && (isName $ source (xs !! 1))
                    then "$" ++ source (xs !! 1) ++ " => "
                    else "")
             ++ (if (length xs > 0) && (isName $ source (xs !! 0))
                    then "$" ++ source (xs !! 0)
                    else "$_") ++ "): ?>"

execute "endforeach" _ = return $ "<?php endforeach; ?>"

--------------------------------------------------------------------------------
-- Output modifier implementations

modify :: Maybe Char -> Expression -> MyParser String

modify (Nothing ) exp = return $ "$this->escape(" ++ result exp ++ ")"
modify (Just 'h') exp = return $ "htmlspecialchars(" ++ result exp ++ ")"
modify (Just 'u') exp = return $ "rawurlencode(" ++ result exp ++ ")"
modify (Just 'r') exp = return $ result exp

--------------------------------------------------------------------------------
-- Utility Functions
join :: [a] -> [[a]] -> [a]
join glue [] = []
join glue xs = foldl1 ((++) . (++ glue)) xs

isName :: String -> Bool
isName [] = False
isName (x:xs) = f x && all g xs where f x = x == '_' || isAlpha x
                                      g x = x == '_' || isAlphaNum x
