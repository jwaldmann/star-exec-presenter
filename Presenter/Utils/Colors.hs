-- | duplicated from templates/solver_result.lucius

module Presenter.Utils.Colors where

import qualified Data.GraphViz.Attributes.Colors as C

colorYes :: C.Color
colorYes =  C.RGB 0x80 0xFF 0xB0
colorNo :: C.Color
colorNo = C.RGB  0x22 0x8B 0x22
colorMaybe :: C.Color
colorMaybe = C.RGB 0xFF 0xFF 0x99
colorBounds :: C.Color
colorBounds = C.RGB 0xB0 0xED 0xEA
colorCertified :: C.Color
colorCertified = C.RGB 0x5C 0xB8 0x5C
colorError :: C.Color
colorError = C.RGB 0xFF 0xCC 0x66
colorNothing :: C.Color
colorNothing = C.RGB 0xFF 0xFF 0xFF
colorAnything :: C.Color
colorAnything = C.RGB 0xA0 0xA0 0xA0
colorOther :: C.Color
colorOther = C.RGB 0xF2 0xCD 0xAC
