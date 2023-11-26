
import XMonad

main :: IO ()
main = do
  xmonad $ def
    { terminal    = myTerm
    , modMask     = myMod
    , borderWidth = 3
    }

myTerm = "wezterm"
myMod = mod4Mask
