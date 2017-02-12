{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE OverloadedStrings        #-}

module Reflex.Dom.SemanticUI.Form where

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

data UiForm = UiForm
  { _uiForm_loading :: Maybe UiLoading
  , _uiForm_status :: Maybe UiStatus
  , _uiForm_equalWidth :: Maybe UiEqualWidth
  , _uiForm_custom :: Maybe Text
  } deriving (Eq, Show)

instance Default UiForm where
  def = UiForm def def def def

instance UiHasLoading UiForm where
  loading f = f { _uiForm_loading = Just UiLoading }

instance UiHasStatus UiForm where
  uiSetStatus s f = f { _uiForm_status = Just s }

instance UiHasEqualWidth UiForm where
  equalWidth f = f { _uiForm_equalWidth = Just UiEqualWidth }

instance UiHasCustom UiForm where
  custom c f = f { _uiForm_custom = addCustom c (_uiForm_custom f) }

uiFormAttrs :: UiForm -> Text
uiFormAttrs UiForm {..} = T.unwords $ catMaybes
  [ uiText <$> _uiForm_loading
  , uiText <$> _uiForm_status
  , uiText <$> _uiForm_equalWidth
  , _uiForm_custom
  ]

uiForm
  :: MonadWidget t m
  => Dynamic t UiForm
  -> m a
  -> m a
uiForm fDyn children = do
    elDynAttr "form" (mkAttrs <$> fDyn) children
  where
    mkAttrs i = "class" =: T.unwords ["ui", uiFormAttrs i, "form"]
