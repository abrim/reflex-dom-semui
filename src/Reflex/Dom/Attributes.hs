{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings        #-}

module Reflex.Dom.Attributes where

import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))

class AttributeSet s where
  setAttr   :: Text -> Text -> s -> s
  addAttr   :: Text -> Text -> s -> s

instance AttributeSet (Map Text Text) where
  setAttr = M.insert
  addAttr =
      M.insertWithKey f
    where
      f k a' a =
        case k of
          "class" -> a <> " " <> a'
          "style" -> a <> ";" <> a'
          _       -> a'
