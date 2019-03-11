module Demo.Coin where

newtype Coin = Coin Int
  deriving (Eq, Show)

maxCoinValue = 1000000

validCoin (Coin n) =
  0 <= n && n <= maxCoinValue

add (Coin a) (Coin b) =
  -- if a+b < maxCoinValue  {- off-by-one error caught by prop_add''' -}
  if a+b <= maxCoinValue
    then Just (Coin (a+b))
    else Nothing
