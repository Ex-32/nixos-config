import qualified System.Exit                 as Exit
import           XMonad
import           XMonad.Config.Desktop       (desktopConfig)
-- import qualified XMonad.Hooks.DynamicLog     as DLog
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.ManageHelpers  (isDialog)
import           XMonad.Hooks.StatusBar      (defToggleStrutsKey, statusBarProp, withEasySB)
import           XMonad.Hooks.StatusBar.PP   
import           XMonad.Layout.Magnifier     (magnifiercz')
import           XMonad.Layout.Spacing       (spacingWithEdge)
import           XMonad.Layout.ThreeColumns  (ThreeCol (ThreeColMid))
import           XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import           XMonad.Util.EZConfig
import           XMonad.Util.SpawnOnce       (spawnOnce)
import           XMonad.Util.Ungrab          (unGrab)

main :: IO ()
main = xmonad
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure def)) defToggleStrutsKey
     $ conf

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

layoutRules = spacingWithEdge 10 $ tiled ||| Mirror tiled ||| Full ||| threeCol
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
  spawnOnce "xmobar"

xmobarConf config =
  withEasySB (statusBarProp "xmobar" (pure xmobarPrettyPrint)) defToggleStrutsKey

xmobarPrettyPrint :: PP
xmobarPrettyPrint = def
  { ppTitleSanitize   = xmobarStrip
  }
  where
    base      = xmobarColor "#1e1e2e" ""
    mantle    = xmobarColor "#181825" ""
    crust     = xmobarColor "#11111b" ""
    subtext0  = xmobarColor "#a6adc8" ""
    subtext1  = xmobarColor "#bac2de" ""
    surface0  = xmobarColor "#313244" ""
    surface1  = xmobarColor "#45475a" ""
    surface2  = xmobarColor "#585b70" ""
    overlay0  = xmobarColor "#6c7086" ""
    overlay1  = xmobarColor "#7f849c" ""
    overlay2  = xmobarColor "#9399b2" ""
    blue      = xmobarColor "#89b4fa" ""
    lavender  = xmobarColor "#b4befe" ""
    sapphire  = xmobarColor "#74c7ec" ""
    sky       = xmobarColor "#89dceb" ""
    teal      = xmobarColor "#94e2d5" ""
    green     = xmobarColor "#a6e3a1" ""
    yellow    = xmobarColor "#f9e2af" ""
    peach     = xmobarColor "#fab387" ""
    maroon    = xmobarColor "#eba0ac" ""
    red       = xmobarColor "#f38ba8" ""
    mauve     = xmobarColor "#cba6f7" ""
    pink      = xmobarColor "#f5c2e7" ""
    flamingo  = xmobarColor "#f2cdcd" ""
    rosewater = xmobarColor "#f5e0dc" ""

term :: String
term = "wezterm"


