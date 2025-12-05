module Lexer where 

import Data.Char 

data Token = TokenNum Int 
           | TokenTrue 
           | TokenFalse
           | TokenPlus 
           | TokenTimes 
           | TokenAnd 
           | TokenOr 
           | TokenLParen 
           | TokenRParen
           | TokenTNum           -- token para tipo Num
           | TokenTBool          -- token para tipo Bool
           | TokenVar String     -- token para variáveis (x, y, etc.)
           | TokenIf             -- token para if
           | TokenThen           -- token para then
           | TokenElse           -- token para else
           | TokenArrow          -- token para seta ->
           | TokenColon          -- token para :
           | TokenLam            -- token para lambda \
           deriving Show 

data Expr = Num Int 
          | BTrue 
          | BFalse 
          | Add Expr Expr 
          | Times Expr Expr 
          | And Expr Expr 
          | Or Expr Expr 
          | Paren Expr 
          | If Expr Expr Expr 
          | Var String
          | Lam String Ty Expr 
          | App Expr Expr 
          deriving Show 

data Ty = TNum 
        | TBool 
        | TFun Ty Ty 
        deriving (Show, Eq) 

lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('*':cs) = TokenTimes : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs  -- ler seta -> 
lexer (':':cs) = TokenColon : lexer cs      -- ler :
lexer ('\\':cs) = TokenLam : lexer cs       -- ler caractere \
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs)
             | isAlpha c = lexKw (c:cs)
lexer _ = error "Lexical error"

lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

lexKw cs = case span isAlpha cs of 
             ("true", rest)  -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest
             ("Num", rest)   -> TokenTNum : lexer rest   -- Num
             ("Bool", rest)  -> TokenTBool : lexer rest  -- Bool
             ("if", rest)    -> TokenIf : lexer rest     -- if
             ("then", rest)  -> TokenThen : lexer rest   -- then
             ("else", rest)  -> TokenElse : lexer rest   -- else
             (var, rest)     -> TokenVar var : lexer rest -- variáveis genéricas