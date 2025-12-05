module Main where 

import Lexer 
import Parser 
import TypeChecker 
import Interpreter 

main :: IO () -- para garantir que o main seja uma ação de input/output
main = getContents >>= print . eval . typecheck . parser . lexer
