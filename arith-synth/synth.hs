-- \| Code synthesis for simple arith exprs.
--
-- Bottom-up brute force.
--
-- To use:
--
-- >>> :l synth
-- [1 of 1] Compiling Main             ( synth.hs, interpreted )
-- Ok, one module loaded.
--
-- >>> head $ synth ex1
-- a - 1
--
-- >>> take 3 $ synth ex2
-- [a,a + 0,0 + a]
--
-- >>> take 2 $ synth ex3
-- [a + 2 + a * a,2 + a + a * a]
--
-- >>> take 4 $ synth ex4
-- [0,0 + 0,a - a,0 - 0]
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), guard)
import Data.Bifunctor (Bifunctor (..))

-- | Number of candidate programs to check
limit :: Int
limit = 1000000

-- | List of valid variables
vars :: [Id]
vars = ["a"]

-- | List of valid literal values
lits :: [Int]
lits = [0 .. 20]

-- | Identifiers
type Id = String

-- | Arithmetic expressions
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Lit Int
  | Var Id
  deriving Eq

-- | Environment, partial map from identifiers to values
type Env = Id -> Maybe Int

instance Show Expr where
  showsPrec :: Int -> Expr -> ShowS
  showsPrec d (Add x y) = showParen (d > p) $ showsPrec p x . showString " + " . showsPrec p y where p = 1
  showsPrec d (Sub x y) = showParen (d > p) $ showsPrec p x . showString " - " . showsPrec p y where p = 1
  showsPrec d (Div x y) = showParen (d > p) $ showsPrec p x . showString " / " . showsPrec p y where p = 2
  showsPrec d (Mul x y) = showParen (d > p) $ showsPrec p x . showString " * " . showsPrec p y where p = 3
  showsPrec _ (Lit i) = showString $ show i
  showsPrec _ (Var x) = showString x

evalArith :: Env -> Expr -> Maybe Int
evalArith env = e
 where
  e :: Expr -> Maybe Int
  e (Add x y) = (+) <$> e x <*> e y
  e (Sub x y) = (-) <$> e x <*> e y
  e (Mul x y) = (*) <$> e x <*> e y
  e (Div x y) = do
    y' <- e y
    guard (y' /= 0)
    div <$> e x <*> return y'
  e (Lit x) = return x
  e (Var x) = env x

genArith :: [Expr]
genArith = concatMap g [1 ..]
 where
  g d
    | d <= 0 = Var <$> vars <|> Lit <$> lits
    | otherwise =
        let d' = d - 1
         in g 0
              <|> Add <$> g d' <*> g d'
              <|> Sub <$> g d' <*> g d'
              <|> Mul <$> g d' <*> g d'
              <|> Div <$> g d' <*> g d'

equivArith :: [Env] -> Expr -> Expr -> Bool
equivArith envs e1 e2 =
  let evals e = flip evalArith e <$> envs
   in e1 == e2 || evals e1 == evals e2

type Input = [(Id, Int)]
type Output = Maybe Int
type Examples = [(Input, Output)]

makeEnv :: Input -> Env
makeEnv ((i, v) : bs) i'
  | i == i' = return v
  | otherwise = makeEnv bs i'
makeEnv [] _ = mzero

check :: [(Env, Output)] -> Expr -> Bool
check ios p = all c ios where c (i, o) = evalArith i p == o

sane :: Examples -> Bool
sane ios = all wf ios && all consistent ios
 where
  consistent (i, o) = all (\(i', o') -> i /= i' || o == o') ios
  wf (i, _) = all ((`elem` vars) . fst) i

synth :: Examples -> [Expr]
synth exs
  | not $ sane exs = []
  | otherwise = [p | p <- take limit genArith, check ios p]
 where
  ios = map (first makeEnv) exs

-- | a - 1
ex1 :: Examples
ex1 =
  [ ([("a", 2)], Just 1)
  , ([("a", 3)], Just 2)
  , ([("a", 4)], Just 3)
  , ([("a", 5)], Just 4)
  , ([("a", 6)], Just 5)
  ]

-- | a
ex2 :: Examples
ex2 =
  [ ([("a", 2)], Just 2)
  , ([("a", 3)], Just 3)
  , ([("a", 4)], Just 4)
  , ([("a", 5)], Just 5)
  , ([("a", 6)], Just 6)
  ]

-- | a * a + a + 2
ex3 :: Examples
ex3 =
  [ ([("a", 2)], Just 8)
  , ([("a", 5)], Just 32)
  , ([("a", 4)], Just 22)
  , ([("a", 6)], Just 44)
  ]

-- | a * 0
ex4 :: Examples
ex4 =
  [ ([("a", 2)], Just 0)
  , ([("a", 3)], Just 0)
  , ([("a", 4)], Just 0)
  ]

-- | 12 / a
ex5 :: Examples
ex5 =
  [ ([("a", 2)], Just 6)
  , ([("a", 3)], Just 4)
  , ([("a", 1)], Just 12)
  , ([("a", 0)], Nothing)
  ]
