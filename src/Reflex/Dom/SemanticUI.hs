{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI
  ( module Reflex.Dom.SemanticUI.Button
  , module Reflex.Dom.SemanticUI.Common
  , module Reflex.Dom.SemanticUI.Dropdown
  , module Reflex.Dom.SemanticUI.Icon
  , module Reflex.Dom.SemanticUI.Input
  , module Reflex.Dom.SemanticUI.Modal
  , module Reflex.Dom.SemanticUI.Label
  , module Reflex.Dom.SemanticUI.Form
  , module Reflex.Dom.SemanticUI.Field
  , module Reflex.Dom.SemanticUI.Container
  , module Reflex.Dom.SemanticUI.Segment
  , module Reflex.Dom.SemanticUI.Grid
  , module Reflex.Dom.Attributes
  , semanticCSS
  ) where

------------------------------------------------------------------------------
import           Data.FileEmbed
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Button
import           Reflex.Dom.SemanticUI.Common
import           Reflex.Dom.SemanticUI.Dropdown
import           Reflex.Dom.SemanticUI.Icon
import           Reflex.Dom.SemanticUI.Input
import           Reflex.Dom.SemanticUI.Modal
import           Reflex.Dom.SemanticUI.Label
import           Reflex.Dom.SemanticUI.Form
import           Reflex.Dom.SemanticUI.Field
import           Reflex.Dom.SemanticUI.Container
import           Reflex.Dom.SemanticUI.Segment
import           Reflex.Dom.SemanticUI.Grid
import           Reflex.Dom.Attributes
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | The contents of the upstream semantic.min.css stylesheet as a Text value.
semanticCSS :: Text
semanticCSS = $(embedStringFile "lib/semantic.min.css")
