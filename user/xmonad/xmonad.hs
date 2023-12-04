import qualified System.Exit                 as Exit
import           XMonad
import           XMonad.Config.Desktop       (desktopConfig)
import           XMonad.Hooks.DynamicLog     (xmobarProp)
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.ManageHelpers  (isDialog)
import           XMonad.Layout.Magnifier     (magnifiercz')
import           XMonad.Layout.ThreeColumns  (ThreeCol (ThreeColMid))
import           XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import           XMonad.Util.EZConfig
import           XMonad.Util.SpawnOnce       (spawnOnce)
import           XMonad.Util.Ungrab          (unGrab)
import           XMonad.Layout.Spacing       (smartSpacingWithEdge)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

main :: IO ()
main = xmonad $ ewmh $ xmobarConf conf

conf = desktopConfig
  { modMask            = mod4Mask
  , terminal           = term
  , borderWidth        = 2
  , normalBorderColor  = "#313244"
  , focusedBorderColor = "#cba6f7"
  , layoutHook         = layoutRules
  , manageHook         = managmentRules
  , startupHook        = startupRules
  } `additionalKeysP` keybinds

keybinds :: [(String, X ())]
keybinds =
  [ ("M-c",   kill)
  , ("M-S-r", spawn "xmonad --restart")
  , ("M-S-q", confirmPrompt def "exit" $ io Exit.exitSuccess)
  , ("M-;",   spawn "xscreensaver-command --lock")

  , ("M-q",         spawn term)
  , ("M-d",         spawn "~/.config/rofi/bin/launcher")
  , ("M-<Print>",   spawn "scrot - | xclip -selection clipboard -target image/png -i")
  , ("M-S-<Print>", unGrab *> spawn "scrot -s - | xclip -selection clipboard -target image/png -i")
  , ("M-<Tab>",     spawn "tdrop -h 82% -w 88% -x 6% -y 9% wezterm start --always-new-process --class floating")

  , ("<XF86AudioMute>",         spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
  , ("<XF86AudioLowerVolume>",  spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
  , ("<XF86AudioRaiseVolume>",  spawn "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+")
  , ("<XF86AudioPrev>",         spawn "playerctl previous")
  , ("<XF86AudioPlay>",         spawn "playerctl play-pause")
  , ("<XF86AudioNext>",         spawn "playerctl next")
  , ("<XF86KbdBrightnessDown>", spawn "brightnessctl set 5%-")
  , ("<XF86KbdBrightnessUp>",   spawn "brightnessctl set 5%+")
  ]

layoutRules = smartSpacingWithEdge 10 $ tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol  = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled     = Tall nmaster delta ratio
    nmaster   = 1      -- Default number of windows in the master pane
    ratio     = 1/2    -- Default proportion of screen occupied by master pane
    delta     = 3/100  -- Percent of screen to increment by when resizing panes

managmentRules :: ManageHook
managmentRules = composeAll
  [ className =? "floating" --> doFloat
  , className =? "Xmessage" --> doFloat
  , isDialog                --> doFloat
  ]

startupRules :: X ()
startupRules = do
  return () >> checkKeymap conf keybinds
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "feh --no-fehbg --bg-fill ~/documents/pictures/wallpaper.png"
  -- spawnOnce "xmobar"

xmobarConf config = 
  withEasySB (statusBarProp "xmobar" (pure xmobarPP)) toggleStrutsKey config

term = "wezterm"

