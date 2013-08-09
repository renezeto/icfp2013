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
size (If0 a b c) = 1 + size a + size b + size c
size (Fold a b c) = 2 + size a + size b + size c
size (Not e) = 1 + size e
size (Shl1 e) = 1 + size e
size (Shr1 e) = 1 + size e
size (Shr4 e) = 1 + size e
size (Shr16 e) = 1 + size e
size (And a b) = 1 + size a + size b
size (Or a b) = 1 + size a + size b
size (Xor a b) = 1 + size a + size b
size (Plus a b) = 1 + size a + size b


-- enumerate (requires a size and TWO OperatorSets (definitely and maybe))

enumerate :: Int -> OperatorSet -> OperatorSet -> [Ast]
enumerate 1 _ mayhave | mayhave `overlapsWith` op_yz = [Zero, One, X, Y, Z]
                      | otherwise = [Zero, One, X]
enumerate 2 musthave mayhave
  | musthave `overlapsWith` ops_binary_trinary = []
  | musthave `overlapsWith` ops_unary =
    case distinctOperators $ intersection musthave ops_unary of
        [myop] -> apply_single_unary myop (enumerate 1 (musthave `difference` myop) mayhave)
        _ -> []
  | otherwise = concatMap thingsfor unaries
      where unaries = filter (overlapsWith ops_unary) $ distinctOperators mayhave
            thingsfor myop = apply_single_unary myop (enumerate 1 (musthave `difference` myop) mayhave)
enumerate 3 musthave mayhave
  | musthave `overlapsWith` ops_trinary = []
  | musthave `overlapsWith` ops_binary && musthave `overlapsWith` ops_unary = []
  | musthave `overlapsWith` ops_binary =
    case distinctOperators $ intersection musthave ops_binary of
        [myop] -> apply_single_binary myop (enumerate 1 (musthave `difference` myop) mayhave)
        _ -> []
  | otherwise = concatMap thingsfor binaries -- ++ similar with unaries here
      where binaries = filter (overlapsWith ops_binary) $ distinctOperators mayhave
            thingsfor myop = apply_single_unary myop (enumerate 1 (musthave `difference` myop) mayhave)

apply_single_binary :: OperatorSet -> [Ast] -> [Ast]
apply_single_binary o xs
  | o == op_plus = [Plus a b | a <- xs, b <- xs]
  | o == op_or = [Or a b | a <- xs, b <- xs]
  | o == op_xor = [Xor a b | a <- xs, b <- xs]
  | o == op_and = [And a b | a <- xs, b <- xs]
  | otherwise = []

apply_single_unary :: OperatorSet -> [Ast] -> [Ast]
apply_single_unary o xs
  | o == op_not = map Not xs
  | o == op_shl1 = map Shl1 xs
  | o == op_shr1 = map Shr1 xs
  | o == op_shr4 = map Shr4 xs
  | o == op_shr16 = map Shr16 xs
  | otherwise = []

-- lisp output


-- OperatorSet code:

ops_trinary = OS (1 + 2)
ops_binary = OS (128 + 256 + 512 + 1024)
ops_unary = OS (4 + 8 + 16 + 32 + 64)

ops_binary_trinary = OS (1 + 2 + 128 + 256 + 512 + 1024)

ops_all = OS (1 + 2 + 4 + 8 + 16 + 32 + 64 + 128 + 256 + 512 + 1024)

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
op_yz = OS 2048
op_tfold = OS 4096

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
          (op_plus, "plus"),
          (op_tfold, "tfold"),
          (op_yz, "yz")]

empty = OS 0

-- union

union :: OperatorSet -> OperatorSet -> OperatorSet
union (OS a) (OS b) = OS (a .|. b)

unions :: [OperatorSet] -> OperatorSet
unions xs = OS $ foldl (.|.) 0 (map tow xs)
  where tow (OS x) = x

difference :: OperatorSet -> OperatorSet -> OperatorSet
difference (OS a) (OS b) = OS (a .&. complement b)

intersection :: OperatorSet -> OperatorSet -> OperatorSet
intersection (OS a) (OS b) = OS (a .&. b)

overlapsWith :: OperatorSet -> OperatorSet -> Bool
a `overlapsWith` b = intersection a b /= empty

distinctOperators :: OperatorSet -> [OperatorSet]
distinctOperators (OS x) = dops 0 x
  where dops :: Int -> Word16 -> [OperatorSet]
        dops _ 0 = []
        dops shiftby z | (z .&. 1) == 1 = OS (unsafeShiftL 1 shiftby) : dops (shiftby + 1) (unsafeShiftR z 1)
                       | otherwise = dops (shiftby + 1) (unsafeShiftR z 1)

instance Show OperatorSet where
    showsPrec _ (OS x) = showString $ unwords $ map snd $ filter ok allops
      where ok (OS o, _) = x .&. o == o

toOperatorSet :: [String] -> OperatorSet
toOperatorSet [] = OS 0
toOperatorSet (x:xs) = xop `union` toOperatorSet xs
  where xop = case lookup x (map swap allops) of Nothing -> error ("bad value: " ++ x)
                                                 Just o -> o

-- add/remove, etc
