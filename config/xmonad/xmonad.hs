module Main (main) where

-- NOTE: Xmonad.Util.Ungrab still needed for versions < 0.18.0
import System.Exit
import System.Taffybar.Support.PagerHints
import XMonad
import XMonad.Actions.CycleWS qualified as CycleWS
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Ungrab

main :: IO ()
main =
    xmonad
        . docks
        . ewmhFullscreen
        . ewmh
        . pagerHints
        $ customConfig

customConfig =
    def
        { modMask = mod4Mask -- Rebind Mod to the Super key
        , layoutHook = customLayoutHook -- Use custom layouts
        , manageHook = customManageHook -- Match on certain windows
        , startupHook = customStartupHook
        , borderWidth = 4
        , normalBorderColor = "#313244"
        , focusedBorderColor = "#cba6f7"
        }
        `keys` [ ("M-c", kill)
               , ("M-S-c", return ()) -- disable default key binding
               , ("M-<Space>", windows W.focusDown)
               , ("M-S-<Space>", windows W.focusUp)
               , ("M-S-<Tab>", return ())
               , ("M-f", sendMessage NextLayout)
               , ("M-<Print>", unGrab *> spawn "@screenshot_full@")
               , ("M-S-<Print>", unGrab *> spawn "@screenshot_select@")
               , ("M-<Tab>", namedScratchpadAction scratchpads "dropterm")
               , ("M-d", spawn "@rofi@")
               , ("M-q", spawn "@kitty@ -1")
               , ("M-S-r", restart "xmonad" True)
               , ("<XF86AudioMute>", spawn "@vol_mute@")
               , ("<XF86AudioLowerVolume>", spawn "@vol_down@")
               , ("<XF86AudioRaiseVolume>", spawn "@vol_up@")
               , ("<XF86AudioPrev>", spawn "@media_prev@")
               , ("<XF86AudioPlay>", spawn "@media_play@")
               , ("<XF86AudioNext>", spawn "@media_next@")
               , ("M1-C-<Delete>", io exitSuccess)
               , ("M-[", CycleWS.prevWS)
               , ("M-]", CycleWS.nextWS)
               , ("M-S-[", CycleWS.shiftToPrev)
               , ("M-S-]", CycleWS.shiftToNext)
               ]
  where
    keys = additionalKeysP

customManageHook :: ManageHook
customManageHook =
    composeAll
        [ className =? "floating" --> doFloat
        , isDialog --> doFloat
        , namedScratchpadManageHook scratchpads
        ]

customLayoutHook = tiled ||| max
  where
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 0.618 -- Default proportion of screen occupied by master pane
    delta = 2 / 100 -- Percent of screen to increment by when resizing panes

    -- parent-children-stack window layout
    tiled =
        renamed [Replace "Tiled"] $
            spacingRaw False (Border 6 6 6 6) True (Border 6 6 6 6) True $
                gaps [(U, 60)] $ -- gap for taffybar
                    smartBorders $
                        Tall nmaster delta ratio

    -- fullscreen monocule layout
    -- (apparently redundant call to smartBorders is for floating windows)
    max = renamed [Replace "Fullscreen"] $ noBorders $ smartBorders Full

scratchpads =
    [ NS "dropterm" "@kitty@ --class dropterm" (className =? "dropterm") $
        customFloating $
            W.RationalRect 0.06 0.09 0.88 0.82 -- x y w h
    ]

-- long running tasks should be started as systemd user services but simple
-- one-shot processes can be run here unless they need special behavior like a
-- reoccurring systemd timer; note that `spawn` will run the program every time
-- xmonad is reloaded, while `spawnOnce` will only run it once
customStartupHook :: X ()
customStartupHook = do
    spawn "@xhost_hack@"
    spawn "@xsetroot_hack@"
