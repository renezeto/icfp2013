module Ast where

import Data.Word ( Word16 )
import Data.Bits ( Bits(..) )

data Ast = Zero | One | X | Y | Z
         | If0 Ast Ast Ast | Fold Ast Ast Ast
         | Not Ast | Shl1 Ast | Shr1 Ast | Shr4 Ast | Shr16 Ast
         | And Ast Ast | Or Ast Ast | Xor Ast Ast | Plus Ast Ast
         deriving ( Read, Show, Eq, Ord )

data OperatorSet = OS Word16


-- eval

--eval :: Ast -> Word64 -> Word64

-- size



-- enumerate (requires a size and TWO OperatorSets (definitely and maybe))


-- lisp output


-- OperatorSet code:

op_if = OS 1
op_fold = OS 2
op_not = OS 4
op_shl1 = OS 8
op_shr1 = OS 16
op_shr4 = OS 32
op_shr16 = OS 64
op_and = OS 128
op_or = OS 256
op_xor = OS 512
op_plus = OS 1024

-- union

union :: OperatorSet -> OperatorSet -> OperatorSet
union (OS a) (OS b) = OS (a .|. b)

difference :: OperatorSet -> OperatorSet -> OperatorSet
difference (OS a) (OS b) = OS (a .&. complement b)

intersection :: OperatorSet -> OperatorSet -> OperatorSet
intersection (OS a) (OS b) = OS (a `xor` b)

-- add/remove, etc
