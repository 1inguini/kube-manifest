module Util (
  (<:>),
  registry,
  rootGid,
  rootUid,
  rootOwn,
  nonrootGid,
  nonrootUid,
  nonrootOwn,
) where

import Secret (host)

import Data.String (IsString)
import System.Posix (GroupID, UserID)

type SemiString s = (IsString s, Semigroup s)

(<:>) :: (IsString s, Monoid s, Eq s) => s -> s -> s
x <:> y
  | x == mempty = y
  | y == mempty = x
  | otherwise = x <> " " <> y

registry :: SemiString s => s
registry = "registry." <> host <> "/library/"

type Owner = (UserID, GroupID)

nonrootOwn :: Owner
nonrootOwn = (nonrootUid, nonrootGid)
nonrootUid :: UserID
nonrootUid = 65532
nonrootGid :: GroupID
nonrootGid = 65532

rootOwn :: Owner
rootOwn = (rootUid, rootGid)
rootUid :: UserID
rootUid = 0
rootGid :: GroupID
rootGid = 0