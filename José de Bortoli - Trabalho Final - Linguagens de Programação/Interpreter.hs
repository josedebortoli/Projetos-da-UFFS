module Interpreter where 

import Lexer 
import Parser 

isValue :: Expr -> Bool 
isValue BTrue  = True 
isValue BFalse = True 
isValue (Num _) = True 
isValue (Lam _ _ _) = True 
isValue (Pair e1 e2) = isValue e1 && isValue e2 -- Par é valor se componentes forem valor
isValue _ = False 

subst :: String -> Expr -> Expr -> Expr 
subst x s y@(Var v) = if x == v then 
                        s 
                      else 
                        y 
subst x s (Num n) = (Num n)
subst x s BTrue = BTrue 
subst x s BFalse = BFalse 
--lógica de Substituição em Abstrações (Lambda)
subst x s (Lam y tp t1) = if x == y then       -- evita substituição se a variável for sombreada, e verifica colisão de nomes: se a variável que queremos substituir (x) é igual ao parâmetro da função (y)
                            Lam y tp t1        -- termina a substituição, daí o parâmetro 'y' faz "shadowing" (sombra) no 'x' externo, protegendo o corpo da função
                          else                 -- se os nomes forem diferentes, não há conflito de escopo
                            Lam y tp (subst x s t1) -- continua a substituição recursivamente dentro do corpo da função (t1)
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2) 
subst x s (Add t1 t2) = Add (subst x s t1) (subst x s t2) 
subst x s (And t1 t2) = And (subst x s t1) (subst x s t2) 
-- Completar subst para outros termos da linguagem

-- propagação recursiva da substituição
-- para multiplicação e ou lógico: a substituição deve ser aplicada tanto no lado esquerdo (t1) quanto no direito (t2)
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2) -- substitui x por s dentro de t1 e t2, mantendo a multiplicação
subst x s (Or t1 t2) = Or (subst x s t1) (subst x s t2)       -- substitui x por s dentro de t1 e t2, mantendo o ou lógico
-- para o If-Then-Else: a substituição deve ocorrer na condição (e), no ramo then (e1) e no ramo else (e2)
subst x s (If e e1 e2) = If (subst x s e) (subst x s e1) (subst x s e2) -- garante que variáveis na condição ou nos ramos sejam atualizadas
-- para parênteses: apenas atravessa os parênteses e aplica a substituição na expressão interna.
subst x s (Paren e) = Paren (subst x s e)                     -- mantém a estrutura de parênteses, aplicando a substituição dentro dela
-- Substituição para Tuplas
subst x s (Pair e1 e2) = Pair (subst x s e1) (subst x s e2)
subst x s (Fst e) = Fst (subst x s e)
subst x s (Snd e) = Snd (subst x s e)

step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = let e2' = step e2
                           in Add (Num n1) e2' 
step (Add e1 e2) = Add (step e1) e2 
-- Implementar step para Times

-- regras de redução (small-step) para multiplicação
step (Times (Num n1) (Num n2)) = Num (n1 * n2)        -- se ambos são números, realiza a multiplicação
step (Times (Num n1) e2) = let e2' = step e2          -- regra 2: se o lado esquerdo já é um valor (Num), tenta reduzir o lado direito (e2).
                             in Times (Num n1) e2'    -- reconstrói a expressão com o lado direito reduzido um passo.
step (Times e1 e2) = Times (step e1) e2               -- regra 1: se o lado esquerdo não é valor, reduz o lado esquerdo primeiro.

step (And BFalse e2) = BFalse 
step (And BTrue e2) = e2 
step (And e1 e2) = And (step e1) e2 
-- Implementar step para Or

-- semântica small-Step para o OU lógico (||)
step (Or BTrue e2) = BTrue      -- curto-circuito: se o primeiro termo é True, o resultado é True imediatamente (ignora e2).
step (Or BFalse e2) = e2        -- se o primeiro termo é False, a expressão se reduz ao segundo termo (e2).
step (Or e1 e2) = Or (step e1) e2 -- regra de congruência: se o lado esquerdo não é um valor, reduz ele um passo primeiro.
-- Implementar step para If

-- semântica small-step para condicional (If-Then-Else)
step (If BTrue e1 e2) = e1       -- regra de computação (True): se a condição avaliou para True, retorna o ramo 'then' (e1).
step (If BFalse e1 e2) = e2      -- regra de computação (False): se a condição avaliou para False, retorna o ramo 'else' (e2).
step (If e e1 e2) = If (step e) e1 e2 -- regra de congruência: se a condição (e) ainda não é um valor final, avalia ela um passo.

step (App (Lam x tp e1) e2) = if (isValue e2) then 
                                subst x e2 e1 
                              else 
                                App (Lam x tp e1) (step e2)
-- regra de congruência para aplicação (E-APP1)
step (App e1 e2) = App (step e1) e2 -- se o lado esquerdo (a função) não é um valor (lambda), avalia ele primeiro, isso força a avaliação da esquerda para a direita.

-- regra de simplificação de parênteses
step (Paren e) = e   -- semanticamente, parênteses não fazem nada, apenas remove o "envelope" e retornamos a expressão de dentro.

-- Steps para Tuplas
step (Pair e1 e2) | not (isValue e1) = Pair (step e1) e2
                  | not (isValue e2) = Pair e1 (step e2)

step (Fst (Pair v1 v2)) | isValue v1 && isValue v2 = v1
step (Fst e) = Fst (step e)

step (Snd (Pair v1 v2)) | isValue v1 && isValue v2 = v2
step (Snd e) = Snd (step e)

eval :: Expr -> Expr
eval e = if isValue e then 
           e
         else 
           eval (step e)