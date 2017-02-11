{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE OverloadedStrings        #-}

module Reflex.Dom.SemanticUI.Grid where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom hiding (fromJSString)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common
------------------------------------------------------------------------------

-- Rewsponsive classes not included

data UiGrid = UiGrid
  { _uiGrid_columns           :: Maybe UiColumns
  , _uiGrid_divided           :: Maybe UiDivided
  , _uiGrid_equalWidth        :: Maybe UiEqualWidth
  , _uiGrid_padded            :: Maybe UiPadded
  , _uiGrid_relaxed           :: Maybe UiRelaxed
  , _uiGrid_centered          :: Maybe UiCentered
  , _uiGrid_alignment         :: Maybe UiAlignment
  , _uiGrid_verticalAlignment :: Maybe UiVerticalAlignment
  , _uiGrid_custom            :: Maybe Text
  } deriving (Eq, Show)

instance Default UiGrid where
  def = UiGrid def def def def def def def def def

instance UiHasColumns UiGrid where
  columns c g = g { _uiGrid_columns = Just $ UiColumns c }

instance UiHasDivided UiGrid where
  uiSetDivided d g = g { _uiGrid_divided = Just d }

instance UiHasEqualWidth UiGrid where
  equalWidth g = g { _uiGrid_equalWidth = Just UiEqualWidth }

instance UiHasPadded UiGrid where
  uiSetPadded p g = g { _uiGrid_padded = Just p }

instance UiHasRelaxed UiGrid where
  relaxed g = g { _uiGrid_relaxed = Just UiRelaxed }

instance UiHasCentered UiGrid where
  centered g = g { _uiGrid_centered = Just UiCentered }

instance UiHasAlignment UiGrid where
  uiSetAlignment a g = g {_uiGrid_alignment = Just a }

instance UiHasVerticalAlignment UiGrid where
  uiSetVerticalAlignment a g = g { _uiGrid_verticalAlignment = Just a }

instance UiHasCustom UiGrid where
  custom c g = g { _uiGrid_custom = addCustom c (_uiGrid_custom g) }

uiGridAttrs :: UiGrid -> Text
uiGridAttrs UiGrid{..} = T.unwords $ catMaybes
  [ uiText <$> _uiGrid_columns
  , uiText <$> _uiGrid_divided
  , uiText <$> _uiGrid_equalWidth
  , uiText <$> _uiGrid_padded
  , uiText <$> _uiGrid_relaxed
  , uiText <$> _uiGrid_centered
  , uiText <$> _uiGrid_alignment
  , uiText <$> _uiGrid_verticalAlignment
  , _uiGrid_custom
  ]

-------------------------------------------------------------------------------
data UiRow = UiRow
  { _uiRow_columns            :: Maybe UiColumns
  , _uiRow_equalWidth         :: Maybe UiEqualWidth
  , _uiRow_stretched          :: Maybe UiStretched
  , _uiRow_color              :: Maybe UiColor
  , _uiRow_centered           :: Maybe UiCentered
  , _uiRow_alignment          :: Maybe UiAlignment
  , _uiRow_verticalAlignment  :: Maybe UiVerticalAlignment
  , _uiRow_custom             :: Maybe Text
  } deriving (Eq, Show)

instance Default UiRow where
  def = UiRow def def def def def def def def

instance UiHasColumns UiRow where
  columns c r = r { _uiRow_columns = Just $ UiColumns c }

instance UiHasEqualWidth UiRow where
  equalWidth r = r { _uiRow_equalWidth = Just UiEqualWidth }

instance UiHasStretched UiRow where
  stretched r = r { _uiRow_stretched = Just UiStretched }

instance UiHasColor UiRow where
  uiSetColor c r = r { _uiRow_color = Just c }

instance UiHasCentered UiRow where
  centered r = r { _uiRow_centered = Just UiCentered }

instance UiHasAlignment UiRow where
  uiSetAlignment a r = r { _uiRow_alignment = Just a }

instance UiHasVerticalAlignment UiRow where
  uiSetVerticalAlignment a r = r { _uiRow_verticalAlignment = Just a }

instance UiHasCustom UiRow where
  custom c r = r { _uiRow_custom = addCustom c (_uiRow_custom r) }

uiRowAttrs :: UiRow -> Text
uiRowAttrs UiRow{..} = T.unwords $ catMaybes
  [ uiText <$> _uiRow_columns
  , uiText <$> _uiRow_equalWidth
  , uiText <$> _uiRow_stretched
  , uiText <$> _uiRow_color
  , uiText <$> _uiRow_centered
  , uiText <$> _uiRow_alignment
  , uiText <$> _uiRow_verticalAlignment
  , _uiRow_custom
  ]

-------------------------------------------------------------------------------
data UiColumn = UiColumn
  { _uiColumn_width             :: Maybe UiWidth
  , _uiColumn_color             :: Maybe UiColor
  , _uiColumn_alignment         :: Maybe UiAlignment
  , _uiColumn_verticalAlignment :: Maybe UiVerticalAlignment
  , _uiColumn_custom            :: Maybe Text
  } deriving (Eq, Show)

instance Default UiColumn where
  def = UiColumn def def def def def

instance UiHasWidth UiColumn where
  width w c = c { _uiColumn_width = Just $ UiWidth w }

instance UiHasColor UiColumn where
  uiSetColor v c = c { _uiColumn_color = Just v }

instance UiHasAlignment UiColumn where
  uiSetAlignment a c = c { _uiColumn_alignment = Just a }

instance UiHasVerticalAlignment UiColumn where
  uiSetVerticalAlignment a c = c { _uiColumn_verticalAlignment = Just a }

instance UiHasCustom UiColumn where
  custom t c = c { _uiColumn_custom = addCustom t (_uiColumn_custom c) }

uiColumnAttrs :: UiColumn -> Text
uiColumnAttrs UiColumn{..} = T.unwords $ catMaybes
  [ uiText <$> _uiColumn_width
  , uiText <$> _uiColumn_color
  , uiText <$> _uiColumn_alignment
  , uiText <$> _uiColumn_verticalAlignment
  , _uiColumn_custom
  ]


-------------------------------------------------------------------------------

uiGrid
  :: MonadWidget t m
  => Dynamic t UiGrid
  -> m a
  -> m a
uiGrid gDyn children = do
    elDynAttr "div" (mkAttrs <$> gDyn) children
  where
    mkAttrs i = "class" =: T.unwords ["ui", uiGridAttrs i, "grid"]

uiRow
  :: MonadWidget t m
  => Dynamic t UiRow
  -> m a
  -> m a
uiRow rDyn children = do
    elDynAttr "div" (mkAttrs <$> rDyn) children
  where
    mkAttrs i = "class" =: T.unwords [uiRowAttrs i, "row"]

uiColumn
  :: MonadWidget t m
  => Dynamic t UiColumn
  -> m a
  -> m a
uiColumn cDyn children = do
    elDynAttr "div" (mkAttrs <$> cDyn) children
  where
    mkAttrs i = "class" =: T.unwords [uiColumnAttrs i, "column"]
