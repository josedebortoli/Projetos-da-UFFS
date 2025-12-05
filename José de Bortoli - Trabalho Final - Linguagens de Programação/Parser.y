{
module Parser where 

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parseError }

%left '+' '-'
%left '*'
%left "||" -- ||
%left "&&" -- &&
%left APP    -- precedência para usar função
%nonassoc if then else -- precedência para ajustar conflitos e definir associatividade

%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    var             { TokenVar $$ } 
 -- novo token para carregar nome de variável
    '+'             { TokenPlus }
    '*'             { TokenTimes }
    "&&"            { TokenAnd }
    "||"            { TokenOr }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    Num             { TokenTNum }    -- Num
    Bool            { TokenTBool }   -- Bool
    if              { TokenIf }      -- if
    then            { TokenThen }    -- then
    else            { TokenElse }    -- else
    '\\'            { TokenLam }     -- \\
    ':'             { TokenColon }   -- :
    "->"            { TokenArrow }   -- ->
    ','             { TokenComma }   -- ,
    fst             { TokenFst }     -- fst
    snd             { TokenSnd }     -- snd


%% 

Exp     : num                       { Num $1 }
        | true                      { BTrue }
        | false                     { BFalse }
        | var                       { Var $1 }     -- regra para variáveis
        | Exp '+' Exp               { Add $1 $3 }
        | Exp '*' Exp               { Times $1 $3 }
        | Exp "&&" Exp              { And $1 $3 }
        | Exp "||" Exp              { Or $1 $3 }
        | Exp Exp %prec APP         { App $1 $2 }                -- regra da aplicação
        | '\\' var ':' Type "->" Exp { Lam $2 $4 $6 }            -- regra do lambda
        | if Exp then Exp else Exp  { If $2 $4 $6 }              -- regra do If
        | '(' Exp ')'               { Paren $2 }
        | '(' Exp ',' Exp ')'       { Pair $2 $4 }               -- regra para Tupla
        | fst Exp                   { Fst $2 }                   -- regra fst
        | snd Exp                   { Snd $2 }                   -- regra snd

Type    : Num                       { TNum } -- regras de definição de tipos
        | Bool                      { TBool }
        | '(' Type "->" Type ')'    { TFun $2 $4 }
        | '(' Type ',' Type ')'     { TPair $2 $4 } -- Tipo Tupla

{ 

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
}