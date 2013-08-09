module Ast where

import Data.Tuple ( swap )
import Data.Word ( Word8, Word16, Word64 )
import Data.Bits ( Bits(..) )

data Ast = Zero | One | X | Y | Z
         | If0 Ast Ast Ast | Fold Ast Ast Ast
         | Not Ast | Shl1 Ast | Shr1 Ast | Shr4 Ast | Shr16 Ast
         | And Ast Ast | Or Ast Ast | Xor Ast Ast | Plus Ast Ast
         deriving ( Read, Show, Eq, Ord )

data OperatorSet = OS Word16
                 deriving ( Eq )


-- eval
eval :: Ast -> Word64 -> Word64 -> Word64 -> Word64
eval Zero _ _ _ = 0
eval One _ _ _ = 1
eval X x _ _ = x
eval Y _ y _ = y
eval Z _ _ z = z
eval (If0 e a b) x y z
  | eval e x y z == 0 = eval a x y z
  | otherwise = eval b x y z
eval (Fold e1 e2 e3) x _ _ = eval e3 x a8 b8
  where a8 = unsafeShiftR a 56 .&. 0xff
        b8 = eval e3 x a7 b7
        a7 = unsafeShiftR a 48 .&. 0xff
        b7 = eval e3 x a6 b6
        a6 = unsafeShiftR a 40 .&. 0xff
        b6 = eval e3 x a5 b5
        a5 = unsafeShiftR a 32 .&. 0xff
        b5 = eval e3 x a4 b4
        a4 = unsafeShiftR a 24 .&. 0xff
        b4 = eval e3 x a3 b3
        a3 = unsafeShiftR a 16 .&. 0xff
        b3 = eval e3 x a2 b2
        a2 = unsafeShiftR a 8 .&. 0xff
        b2 = eval e3 x a1 b1
        a1 = a .&. 0xff
        b1 = eval e2 x 0 0
        a = eval e1 x 0 0
eval (Not e) x y z = complement (eval e x y z)
eval (Shl1 e) x y z = unsafeShiftL (eval e x y z) 1
eval (Shr1 e) x y z = unsafeShiftR (eval e x y z) 1
eval (Shr4 e) x y z = unsafeShiftR (eval e x y z) 4
eval (Shr16 e) x y z = unsafeShiftR (eval e x y z) 16
eval (And e1 e2) x y z = (eval e1 x y z) .&. (eval e2 x y z)
eval (Or e1 e2) x y z = (eval e1 x y z) .|. (eval e2 x y z)
eval (Xor e1 e2) x y z = (eval e1 x y z) `xor` (eval e2 x y z)
eval (Plus e1 e2) x y z = (eval e1 x y z) + (eval e2 x y z)





-- size

size :: Ast -> Int
size Zero = 1
size One = 1
size X = 1
size Y = 1
size Z = 1

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

allops = [(op_if, "if0"),
          (op_fold, "fold"),
          (op_not, "not"),
          (op_shl1, "shl1"),
          (op_shr1, "shr1"),
          (op_shr4, "shr4"),
          (op_shr16, "shr16"),
          (op_and, "and"),
          (op_or, "or"),
          (op_xor, "xor"),
          (op_plus, "plus")]

-- union

union :: OperatorSet -> OperatorSet -> OperatorSet
union (OS a) (OS b) = OS (a .|. b)

difference :: OperatorSet -> OperatorSet -> OperatorSet
difference (OS a) (OS b) = OS (a .&. complement b)

intersection :: OperatorSet -> OperatorSet -> OperatorSet
intersection (OS a) (OS b) = OS (a `xor` b)

instance Show OperatorSet where
    showsPrec _ (OS x) = showString $ unwords $ map snd $ filter ok allops
      where ok (OS o, _) = x .&. o == o

toOperatorSet :: [String] -> OperatorSet
toOperatorSet [] = OS 0
toOperatorSet (x:xs) = xop `union` toOperatorSet xs
  where xop = case lookup x (map swap allops) of Nothing -> error ("bad value: " ++ x)
                                                 Just o -> o

-- add/remove, etc
