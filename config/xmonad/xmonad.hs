
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
-- NOTE: Importing XMonad.Util.Ungrab is only necessary for versions
-- < 0.18.0! For 0.18.0 and up, this is already included in the
-- XMonad import and will generate a warning instead!
import XMonad.Util.Ungrab

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders 

import Graphics.X11.ExtraTypes.XF86

import System.Taffybar.Support.PagerHints (pagerHints)



main :: IO ()
main = xmonad
     . docks
     . ewmhFullscreen
     . ewmh
     . pagerHints
     $ myConfig


myConfig = def
    { modMask     = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook  = myLayout      -- Use custom layouts
    , manageHook  = myManageHook  -- Match on certain windows
    , borderWidth = 4
    , normalBorderColor = "#313244"
    , focusedBorderColor = "#cba6f7"
    }
  `additionalKeysP`
    [ ("M-c"        , kill)
    , ("M-S-c"      , return ())
    , ("M-<Print>"  , unGrab *> spawn "@screenshot_full@")
    , ("M-S-<Print>", unGrab *> spawn "@screenshot_select@")
    , ("M-d"        , spawn "@rofi@")
    , ("M-q"        , spawn "@kitty_mono@")
    ]
  `additionalKeys`
    [ ((0, xF86XK_AudioMute)       , spawn "@vol_mute@")
    , ((0, xF86XK_AudioLowerVolume), spawn "@vol_down@")
    , ((0, xF86XK_AudioRaiseVolume), spawn "@vol_up@")
    , ((0, xF86XK_AudioPrev)       , spawn "@media_prev@")
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "floating" --> doFloat
    , isDialog                --> doFloat
    ]

myLayout = tiled ||| max
  where
    tiled      = spacingRaw False (Border 6 6 6 6) True (Border 6 6 6 6) True 
               $ gaps [(U, 60)] 
               $ Tall nmaster delta ratio
    max        = noBorders Full
    nmaster    = 1      -- Default number of windows in the master pane
    ratio      = 0.618 -- Default proportion of screen occupied by master pane
    delta      = 2/100  -- Percent of screen to increment by when resizing panes

