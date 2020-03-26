module Block5
  ( ArithmeticError (..)
  , Expr (..)
  , eval
  ) where

-- Task 1

-- | Type which represents a simple recursive
-- algebraic computation
data Expr
  = Const Int
  | Sum Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show)

-- | Type which represents an error
-- occured while evaluating an 'Expr'
data ArithmeticError
  = ArithmeticError { message :: String }
  deriving (Eq, Show)

-- | Function which evaluates 'Expr'
-- return either result or ArithmeticError
-- error occures on division by zero
-- and negative power
eval :: Expr -> Either ArithmeticError Int
eval e =
  case e of
    Const x -> Right x
    Sum l r -> operate (+) l r (const Nothing)
    Sub l r -> operate (-) l r (const Nothing)
    Mul l r -> operate (*) l r (const Nothing)
    Div l r -> operate (div) l r (checker (== 0) "Division by zero")
    Pow l r -> operate (^) l r (checker (< 0) "Negative power")
  where
    checker
      :: (Int -> Bool)
      -> String
      -> Int
      -> Maybe String
    checker cond name x =
      if cond x
      then Just name
      else Nothing

    operate
      :: (Int -> Int -> Int)
      -> Expr
      -> Expr
      -> (Int -> Maybe String)
      -> Either ArithmeticError Int
    operate f l r check =
      case eval l of
        err@(Left _) -> err
        Right x ->
          case eval r of
            err@(Left _) -> err
            Right y ->
              case check y of
                Just m  -> Left (ArithmeticError { message = m })
                Nothing -> Right (f x y)

-- Task 2

-- This task is too hard to fit in deadline and i don't know how to implement it
