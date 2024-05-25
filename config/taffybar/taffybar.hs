module Main (main) where

import Data.Bits
import Data.Default (def)
import Data.Text qualified as Text
import System.Taffybar
import System.Taffybar.Context (TaffybarConfig (..))
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph

main :: IO ()
main = startTaffybar taffyConfig

baseGraphConfig, netCfg, memCfg, cpuCfg :: GraphConfig
baseGraphConfig =
  def
    { graphPadding = 0
    , graphBorderWidth = 0
    , graphWidth = 75
    , graphBackgroundColor = (0.0, 0.0, 0.0, 0.0)
    , graphDirection = RIGHT_TO_LEFT
    }
netCfg =
  baseGraphConfig
    { graphDataColors =
        [ hex 0xf5c2e7
        , hex 0xcba6f7
        ]
    , graphLabel = Just $ Text.pack "NET"
    }
memCfg =
  baseGraphConfig
    { graphDataColors = [hex 0x89b4fa]
    , graphLabel = Just $ Text.pack "RAM"
    }
cpuCfg =
  baseGraphConfig
    { graphDataColors =
        [ hex 0xa6e3a1
        , hexA 0xf9e2af 0.75
        ]
    , graphLabel = Just $ Text.pack "CPU"
    }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

taffyConfig :: TaffybarConfig
taffyConfig =
  withBatteryRefresh $
    withLogServer $
      withToggleServer $
        toTaffyConfig
          def
            { startWidgets = [workspaces]
            , centerWidgets = [windowName >>= buildContentsBox]
            , endWidgets =
                map
                  (>>= buildContentsBox)
                  [ sniTrayNew
                  , batteryIconNew
                  , clock
                  , cpu
                  , mem
                  , net
                  , mpris2New
                  ]
            , barPosition = Top
            , barPadding = 6
            , barHeight = ExactSize 48
            , widgetSpacing = 0
            }
 where
  workspaces =
    workspacesNew $
      def
        { minIcons = 1
        , widgetGap = 0
        , showWorkspaceFn = workspaceFilter
        }
  cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
  mem = pollingGraphNew memCfg 1 memCallback
  net = networkGraphNew netCfg Nothing
  clock =
    textClockNewWith
      def
        { clockFormatString = "%a %Y-%m-%d %H:%M%z"
        }
  windowName =
    windowsNew
      def
        { getMenuLabel = truncatedGetMenuLabel truncate
        , getActiveLabel = truncatedGetActiveLabel truncate
        }
   where
    truncate = 80

hex :: Int -> (Double, Double, Double, Double)
hex x = hexA x 1.0

hexA :: Int -> Double -> (Double, Double, Double, Double)
hexA x a =
  ( convert $ x `shiftR` 16
  , convert $ x `shiftR` 8
  , convert x
  , a
  )
 where
  convert x = fromIntegral (x .&. 0xFF) / 255.0

workspaceFilter :: Workspace -> Bool
workspaceFilter ws = workspaceName ws /= "NSP" && hideEmpty ws
