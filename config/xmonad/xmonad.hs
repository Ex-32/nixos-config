module Main (main) where

-- NOTE: Xmonad.Util.Ungrab still needed for versions < 0.18.0
import System.Exit
import XMonad
import XMonad.Actions.CycleWS qualified as CycleWS
import XMonad.Actions.MouseGestures (mouseGesture)
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
import XMonad.Util.Run
import XMonad.Util.SessionStart
import XMonad.Util.Ungrab

main :: IO ()
main =
    xmonad
        . docks
        . ewmhFullscreen
        . ewmh
        $ customConfig

customConfig =
    def
        { modMask = mod4Mask -- Rebind Mod to the Super key
        , layoutHook = customLayoutHook -- Use custom layouts
        , manageHook = customManageHook -- Match on certain windows
        , startupHook = customStartupHook
        , borderWidth = 2
        , normalBorderColor = "#11111b"
        , focusedBorderColor = "#cba6f7"
        }
        `keys` [ ("M-c", kill)
               , ("M-S-c", return ()) -- disable default key binding
               , ("M-<Space>", windows W.focusDown)
               , ("M-S-<Space>", windows W.focusUp)
               , ("M-S-<Tab>", return ())
               , ("M-f", sendMessage NextLayout)
               , ("M-<Print>", unGrab *> unsafeSpawn "@screenshot_full@")
               , ("M-S-<Print>", unGrab *> unsafeSpawn "@screenshot_select@")
               , ("M-<Tab>", namedScratchpadAction scratchpads "dropterm")
               , ("M-d", unGrab *> unsafeSpawn "@rofi@")
               , ("M-q", safeSpawn "@kitty@" ["-1"])
               , ("M-S-r", restart "xmonad" True)
               , ("<XF86AudioMute>", safeSpawn "@wpctl@" ["set-mute", "@DEFAULT_AUDIO_SINK@", "toggle"])
               , ("<XF86AudioLowerVolume>", safeSpawn "@vol_down@" [])
               , ("<XF86AudioRaiseVolume>", safeSpawn "@vol_up@" [])
               , ("<XF86AudioPrev>", safeSpawn "@playerctl@" ["previous"])
               , ("<XF86AudioPlay>", safeSpawn "@playerctl@" ["play-pause"])
               , ("<XF86AudioNext>", safeSpawn "@playerctl@" ["next"])
               , ("<XF86MonBrightnessDown>", safeSpawn "@brightnessctl@" ["set", "5%-"])
               , ("<XF86MonBrightnessUp>", safeSpawn "@brightnessctl@" ["set", "5%+"])
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
    ratio = phi - 1 -- Default proportion of screen occupied by master pane
    delta = 2 / 100 -- Percent of screen to increment by when resizing panes

    -- parent-children-stack window layout
    tiled =
        renamed [Replace "Tiled"] $
            smartBorders $
                Tall nmaster delta $ toRational ratio

    -- fullscreen monocule layout
    -- (apparently redundant call to smartBorders is for floating windows)
    max = renamed [Replace "Fullscreen"] $ noBorders $ smartBorders Full

scratchpads =
    [ NS "dropterm" "@kitty@ -o background_opacity=0.7 --class dropterm" (className =? "dropterm") $
        customFloating $
            W.RationalRect 0.06 0.09 0.88 0.82 -- x y w h
    ]

-- long running tasks should be started as systemd user services but simple
-- one-shot processes can be run here unless they need special behavior like a
-- reoccurring systemd timer; note that `spawn` will run the program every time
-- xmonad is reloaded, while `spawnOnce` will only run it once
customStartupHook :: X ()
customStartupHook = do
    safeSpawn "@xhost@" ["+SI:localuser:root"]
    safeSpawn "@xsetrootk@" ["-cursor_name", "left_ptr"]
    safeSpawn "@wpctl@" ["set-volume", "@DEFAULT_AUDIO_SOURCE@", "30%"]
    safeSpawnOnce "@brightnessctl@" ["set", "75%"]

phi :: Double
phi = (1.0 + sqrt 5.0) / 2

safeSpawnOnce :: FilePath -> [String] -> X ()
safeSpawnOnce cmd args = doOnce $ safeSpawn cmd args
