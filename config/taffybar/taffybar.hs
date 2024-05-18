module Main where

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

transparent
    , yellow1
    , yellow2
    , green1
    , green2
    , taffyBlue ::
        (Double, Double, Double, Double)
transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)

baseGraphConfig, netCfg, memCfg, cpuCfg :: GraphConfig
baseGraphConfig =
    def
        { graphPadding = 0
        , graphBorderWidth = 0
        , graphWidth = 75
        , graphBackgroundColor = transparent
        , graphDirection = RIGHT_TO_LEFT
        }
netCfg =
    baseGraphConfig
        { graphDataColors = [yellow1, yellow2]
        , graphLabel = Just $ Text.pack "NET"
        }
memCfg =
    baseGraphConfig
        { graphDataColors = [taffyBlue]
        , graphLabel = Just $ Text.pack "RAM"
        }
cpuCfg =
    baseGraphConfig
        { graphDataColors = [green1, green2]
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

workspaceFilter :: Workspace -> Bool
workspaceFilter ws = workspaceName ws /= "NSP" && hideEmpty ws
