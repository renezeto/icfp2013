module Ast where

data Ast = Zero | One | X | Y | Z
         | If0 Ast Ast Ast
         | Fold Ast Ast Ast
         | Not Ast
         | Shl1 Ast
         | Shr1 Ast
         | Shr4 Ast
         | Shr16 Ast
         | And Ast Ast
         | Or Ast Ast
         | Xor Ast Ast
         | Plus Ast Ast
         deriving ( Read, Show, Eq, Ord )

data Operator = Ifop | Foldop | Notop | Shl1op | Shr1op
              | Shr4op | Shr16op | Andop | Orop | Xorop | Plusop

-- eval



-- size



-- enumerate
