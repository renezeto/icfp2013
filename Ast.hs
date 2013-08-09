module Ast where

import Data.Word ( Word64 )

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

data OperatorSet = Ifop | Foldop | Notop | Shl1op | Shr1op
              | Shr4op | Shr16op | Andop | Orop | Xorop | Plusop



-- eval

--eval :: Ast -> Word64 -> Word64

-- size



-- enumerate (requires a size and TWO OperatorSets (definitely and maybe))


-- lisp output


-- OperatorSet code:

-- union

-- difference

-- add/remove, etc
