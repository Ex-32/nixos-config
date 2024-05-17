
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Gaps
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders 
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.StackSet (RationalRect(RationalRect))
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad 
import XMonad.Util.Ungrab -- NOTE: still needed for versions < 0.18.0

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
    , startupHook = myStartupHook
    , borderWidth = 4
    , normalBorderColor = "#313244"
    , focusedBorderColor = "#cba6f7"
    }
  `additionalKeysP`
    -- window management key bindings
    [ ("M-c"  , kill)
    , ("M-S-c", return ()) -- disable default key binding

    -- task spawning key bindings
    , ("M-<Print>"  , unGrab *> spawn "@screenshot_full@")
    , ("M-<Tab>"    , namedScratchpadAction scratchpads "dropterm")
    , ("M-S-<Print>", unGrab *> spawn "@screenshot_select@")
    , ("M-d"        , spawn "@rofi@")
    , ("M-q"        , spawn "@kitty@ -1")

    -- media key bindings
    , ("<XF86AudioMute>"       , spawn "@vol_mute@")
    , ("<XF86AudioLowerVolume>", spawn "@vol_down@")
    , ("<XF86AudioRaiseVolume>", spawn "@vol_up@")
    , ("<XF86AudioPrev>"       , spawn "@media_prev@")
    , ("<XF86AudioPlay>"       , spawn "@media_play@")
    , ("<XF86AudioNext>"       , spawn "@media_next@")
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "floating" --> doFloat
    , isDialog                --> doFloat
    , namedScratchpadManageHook scratchpads
    ]

myLayout = tiled ||| max
  where
    tiled      = spacingRaw False (Border 6 6 6 6) True (Border 6 6 6 6) True 
               $ gaps [(U, 60)] 
               $ smartBorders
               $ Tall nmaster delta ratio
    max        = noBorders $ smartBorders Full
    nmaster    = 1     -- Default number of windows in the master pane
    ratio      = 0.618 -- Default proportion of screen occupied by master pane
    delta      = 2/100 -- Percent of screen to increment by when resizing panes

scratchpads = 
    [ NS "dropterm" "@kitty@ --class dropterm" (className =? "dropterm") 
        $ customFloating 
        $ RationalRect 0.06 0.09 0.88 0.82 -- x y w h
    ]

-- long running tasks should be started as systemd user services but simple
-- one-shot processes can be run here unless they need special behavior like a
-- reoccurring systemd timer; note that `spawn` will run the program every time
-- xmonad is reloaded, while `spawnOnce` will only run it once
myStartupHook :: X ()
myStartupHook = do
  spawn "@xhost_hack@"

