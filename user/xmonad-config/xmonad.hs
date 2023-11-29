
import qualified System.Exit as Exit

import XMonad
import XMonad.Layout.ThreeColumns
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
-- import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook

import Graphics.X11.ExtraTypes.XF86

main :: IO ()
main = xmonad $ ewmh $ xmobarProp desktopConfig
    { modMask            = mod4Mask
    , terminal           = term
    , borderWidth        = 2
    , normalBorderColor  = "#313244"
    , focusedBorderColor = "#cba6f7"
    , layoutHook         = layoutRules
    , manageHook         = managmentRules
    , startupHook        = startupRules
    }
  `additionalKeysP`
    [ ("M-d",     spawn "~/.config/rofi/bin/launcher")
    , ("M-q",     spawn term)
    , ("M-<Tab>", spawn "tdrop -h 76% -w 86% -x 7% -y 12% wezterm start --always-new-process --class floating")
    , ("M-S-r",   spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    ]
  `additionalKeys`
    [ ((0, xF86XK_AudioMute),         spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
    , ((0, xF86XK_AudioLowerVolume),  spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
    , ((0, xF86XK_AudioRaiseVolume),  spawn "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+")
    , ((0, xF86XK_AudioPrev),         spawn "playerctl previous")
    , ((0, xF86XK_AudioPlay),         spawn "playerctl play-pause")
    , ((0, xF86XK_AudioNext),         spawn "playerctl next")
    , ((0, xF86XK_KbdBrightnessDown), spawn "brightnessctl set 5%-")
    , ((0, xF86XK_KbdBrightnessUp),   spawn "brightnessctl set 5%+")
    ]

layoutRules = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

managmentRules :: ManageHook
managmentRules = composeAll
    [ className =? "floating" --> doFloat
    , isDialog                --> doFloat
    ]

startupRules :: X ()
startupRules = do
  spawnOnce "picom"

term = "wezterm"
