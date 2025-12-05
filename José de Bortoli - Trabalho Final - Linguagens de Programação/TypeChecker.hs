module TypeChecker where 

import Lexer 

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty 
typeof ctx BTrue = Just TBool 
typeof ctx BFalse = Just TBool 
typeof ctx (Num n) = Just TNum 
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TNum, Just TNum) -> Just TNum 
                           _                      -> Nothing
-- Implementar typeof para Times 
typeof ctx (Times e1 e2) = case (typeof ctx e1, typeof ctx e2) of -- regra para multiplicação, aqui analisa os tipos dos dois operandos de forma recursiva (esquerda e direita)
                           (Just TNum, Just TNum) -> Just TNum    -- se ambos forem números (TNum), a operação é válida e retorna TNum
                           _                      -> Nothing      -- se qualquer um não for número, rejeita a expressão (Erro de Tipo)
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TBool, Just TBool) -> Just TBool 
                           _                        -> Nothing
-- Implementar typeof para Or 
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of    -- regra para disjunção lógica, aqui analisa os tipos das duas sub-expressões lógicas de forma recursiva
                           (Just TBool, Just TBool) -> Just TBool -- se ambos os lados forem booleanos, o resultado é um booleano válido
                           _                        -> Nothing    -- se algum dos lados não for Bool (ex: um número), é um erro de tipo
typeof ctx (If e e1 e2) = case typeof ctx e of 
                            Just TBool -> case (typeof ctx e1, typeof ctx e2) of 
                                            (Just t1, Just t2) | t1 == t2  -> Just t1 
                                                               | otherwise -> Nothing 
                                            _ -> Nothing  
                            _ -> Nothing 
typeof ctx (Var x) = lookup x ctx 
typeof ctx (Lam x tp b) = let ctx' = (x,tp) : ctx 
                            in case (typeof ctx' b) of 
                                 Just tr -> Just (TFun tp tr)
                                 _ -> Nothing 
typeof ctx (App e1 e2) = case typeof ctx e1 of 
                           Just (TFun tp tr) -> case typeof ctx e2 of 
                                                  Just t2 | t2 == tp -> Just tr 
                                                  _ -> Nothing 
                           _ -> Nothing 
typeof ctx (Paren e) = typeof ctx e  -- novo typeof para usar em expressões que usam parênteses

-- Regras de Tipos para Tuplas
typeof ctx (Pair e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                            (Just t1, Just t2) -> Just (TPair t1 t2) -- deu certo: retorna um tipo produto (T1, T2)
                            _                  -> Nothing -- falha: Um dos elementos tem erro de tipo
typeof ctx (Fst e) = case typeof ctx e of 
                       Just (TPair t1 t2) -> Just t1 -- retorna o tipo do primeiro componente
                       _                  -> Nothing -- erro: tenta usar fst em algo que não é tupla (ex: fst 5)
typeof ctx (Snd e) = case typeof ctx e of 
                       Just (TPair t1 t2) -> Just t2 -- retorna o tipo do segundo componente
                       _                  -> Nothing 

typecheck :: Expr -> Expr 
typecheck e = case typeof [] e of 
                Just _ -> e 
                _      -> error "Type error!"