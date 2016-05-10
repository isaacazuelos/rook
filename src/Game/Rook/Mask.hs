-- |
-- Module      : Game.Rook.Mask
-- Description : A board mask
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--

module Game.Rook.Mask
    (
      Mask
    , empty
    -- * Operations
    , intersection
    , union
    , remove
    , invert
    )
  where

newtype Mask = Mask ()

empty :: Mask
empty = undefined

intersection :: Mask -> Mask -> Mask
intersection = undefined

union :: Mask -> Mask -> Mask
union = undefined

remove :: Mask -> Mask -> Mask
remove = undefined

invert :: Mask -> Mask
invert = undefined
