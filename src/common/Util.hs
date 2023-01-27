module Util (
  registry,
) where

import Data.Text (Text)
import Secret (host)

registry :: Text
registry = "registry." <> host <> "/library/"