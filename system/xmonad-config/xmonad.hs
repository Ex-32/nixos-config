
import XMonad

main :: IO ()
main = do
  xmonad $ def
    { terminal    = term
    , modMask     = mod
    , borderWidth = 3
    }

term = "wezterm"
mod = mod4Mask
