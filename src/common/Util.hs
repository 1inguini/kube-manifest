module Util (
  (<:>),
  registry,
) where

import Secret (host)

import Data.String (IsString)

type SemiString s = (IsString s, Semigroup s)

(<:>) :: (IsString s, Monoid s, Eq s) => s -> s -> s
x <:> y
  | x == mempty = y
  | y == mempty = x
  | otherwise = x <> " " <> y

registry :: SemiString s => s
registry = "registry." <> host <> "/library/"