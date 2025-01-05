module Main (main) where

import System.Exit
import XMonad
import XMonad.Actions.CycleWS qualified as CycleWS
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Operations
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SessionStart

phi :: Double
phi = (1.0 + sqrt 5.0) / 2

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
        , normalBorderColor = "#252726"
        , focusedBorderColor = "#ff006e"
        }
        `keys` [ ("M-c", kill)
               , ("M-S-c", return ()) -- disable default key binding
               , ("M-<Space>", windows W.focusDown)
               , ("M-S-<Space>", windows W.focusUp)
               , ("M-S-<Tab>", return ())
               , ("M-f", sendMessage NextLayout)
               , ("M-<Print>", unGrab >> unsafeSpawn "@screenshot_full@")
               , ("M-S-<Print>", unGrab >> unsafeSpawn "@screenshot_select@")
               , ("M-S-w", safeSpawn "@change_wallpaper@" [])
               , ("M-<Tab>", namedScratchpadAction scratchpads "dropterm")
               , ("M-q", safeSpawn "@ghostty@" [])
               , ("M-S-r", restart "xmonad" True)
               , ("<XF86AudioMute>", safeSpawn "@wpctl@" ["set-mute", "\x40\&DEFAULT_AUDIO_SINK\x40", "toggle"])
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
               ,
                   ( "M-d"
                   , safeSpawn
                        "@rlaunch@"
                        [ "--font=FiraCode Nerd Font"
                        , "-h48"
                        , "--color0=#100c00"
                        , "--color1=#ff006e"
                        , "--color2=#bdadb8"
                        , "--color3=#fcfcfc"
                        , "--color4=#100c00"
                        ]
                   )
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
            spacingRaw False (Border 3 3 3 3) True (Border 3 3 3 3) True $
                gaps [(U, 48)] $
                    smartBorders $
                        Tall nmaster delta $
                            toRational ratio

    -- fullscreen monocule layout
    -- (apparently redundant call to smartBorders is for floating windows)
    max = renamed [Replace "Fullscreen"] $ noBorders $ smartBorders Full

scratchpads =
    [ NS
        "dropterm"
        "@ghostty@ --background-opacity=0.75 --class=ghostty.dropterm"
        (className =? "ghostty.dropterm")
        $ customFloating
        $ W.RationalRect 0.06 0.09 0.88 0.82 -- x y w h
    ]

-- long running tasks should be started as systemd user services but simple
-- one-shot processes can be run here unless they need special behavior like a
-- reoccurring systemd timer; note that `spawn` will run the program every time
-- xmonad is reloaded, while `spawnOnce` will only run it once
customStartupHook :: X ()
customStartupHook = do
    safeSpawn "@xhost@" ["+SI:localuser:root"]
    safeSpawn "@xsetroot@" ["-cursor_name", "left_ptr"]
    -- safeSpawn "@wpctl@" ["set-volume", "\x40\&DEFAULT_AUDIO_SOURCE\x40", "30%"]
    safeSpawnOnce "@brightnessctl@" ["set", "75%"]

safeSpawnOnce :: FilePath -> [String] -> X ()
safeSpawnOnce cmd args = doOnce $ safeSpawn cmd args
