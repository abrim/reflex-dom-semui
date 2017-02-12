{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE OverloadedStrings        #-}

module Reflex.Dom.SemanticUI.Field where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom hiding (fromJSString)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common
import           Reflex.Dom.SemanticUI.Icon
------------------------------------------------------------------------------

data UiField = UiField
  { _uiField_inline   :: Maybe UiInline
  , _uiField_disabled :: Maybe UiDisabled
  , _uiField_status   :: Maybe UiStatus
  , _uiField_width    :: Maybe UiWidth
  , _uiField_required :: Maybe UiRequired
  , _uiField_custom   :: Maybe Text
  } deriving (Eq, Show)

instance Default UiField where
  def = UiField def def def def def def

instance UiHasInline UiField where
  inline f = f { _uiField_inline = Just UiInline }

instance UiHasDisabled UiField where
  disabled f = f { _uiField_disabled = Just UiDisabled }

instance UiHasStatus UiField where
  uiSetStatus s f = f { _uiField_status = Just s }

instance UiHasWidth UiField where
  width w f = f { _uiField_width = Just $ UiWidth w }

instance UiHasRequired UiField where
  required f = f { _uiField_required = Just UiRequired }

uiFieldAttrs :: UiField -> Text
uiFieldAttrs UiField{..} = T.unwords $ catMaybes
  [ uiText <$> _uiField_inline
  , uiText <$> _uiField_disabled
  , uiText <$> _uiField_status
  , uiText <$> _uiField_width
  , uiText <$> _uiField_required
  , _uiField_custom
  ]

uiField
  :: MonadWidget t m
  => Dynamic t UiField
  -> m a
  -> m a
uiField fDyn children = do
    elDynAttr "div" (mkAttrs <$> fDyn) children
  where
    mkAttrs i = "class" =: T.unwords [uiFieldAttrs i, "field"]
