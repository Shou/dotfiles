import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.XMonadLog
import System.Taffybar.SimpleClock
import System.Taffybar.Battery
--import System.Taffybar.Name
import System.Taffybar.Widgets.PollingGraph
import System.Information.CPU

import Control.Monad.IO.Class
import Control.Monad
import Data.String.Utils (split)
import System.Process (readProcess, runCommand)

import Graphics.UI.Gtk

name :: IO Widget
name = do
    label <- labelNew Nothing
    name <- getName
    labelSetMarkup label $ "<span font='Terminus 8'>" ++ name ++  "</span>"
    widgetShowAll label
    miscSetPadding label 5 0
    return (toWidget label)

expertLog :: IO Widget
expertLog = do
    -- xmonad logging, workspaces, etc
    log <- xmonadLogNew

    -- Haskell logo
    pixbuf <- pixbufNewFromFileAtScale "/home/suwako/.xmonad/icons/Haskell-logo-white.svg" (-1) 12 True
    icon <- imageNewFromPixbuf pixbuf
    miscSetPadding icon 5 0

    -- hbox to add widgets to
    hbox <- hBoxNew False 1
    boxPackStart hbox icon PackNatural 0
    boxPackEnd hbox log PackNatural 0

    -- event box for mouse scrolling
    ebox <- eventBoxNew
    ebox `containerAdd` hbox
    ebox `eventBoxSetVisibleWindow` False
    ebox `on` scrollEvent $ tryEvent $ do
        dir <- eventScrollDirection
        case dir of
            ScrollUp    -> void . liftIO $ runCommand "xmonad-ws prev 2>/dev/null" 
            ScrollDown  -> void . liftIO $ runCommand "xmonad-ws next 2>/dev/null" 
            ScrollLeft  -> return ()
            ScrollRight -> return ()

    widgetShowAll ebox
    return (toWidget ebox)

expertClock :: IO Widget
expertClock = do
    clock <- textClockNew Nothing "<span font='Terminus 8' fgcolor='#fcfcfc'>%a %b %_d, %H:%M:%S</span>" 1
    return clock

cpuCallback = do
    (_, systemLoad, totalLoad) <- cpuLoad
    return [ totalLoad, systemLoad ]

main = do
    let cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                        , (1, 0, 1, 0.5)
                                                        ]
                                    , graphLabel = Just "cpu"
                                    }
        clock = expertClock
        log = expertLog
        tray = systrayNew
        battery = textBatteryNew "<span face='terminus' size='8' weight='bold'>%d%%</span>" 15
        cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
    defaultTaffybar defaultTaffybarConfig { startWidgets    = [ log ]
                                          , endWidgets      = [ name, battery, tray ]
                                          , centerWidget    = clock
                                          , barHeight       = 16
                                          }

-- Utils
getName :: IO String
getName = do
    user <- fmap (\x -> lines x !! 0) $ readProcess "/usr/bin/whoami" [] ""
    contents <- readFile "/etc/passwd"
    return (contents `findNameLine` user)
  where findNameLine :: String -> String -> String
        findNameLine x y = let name = getName . filter (\x' -> take (length y) x' == y) $ lines x
                           in if null name then y else name
        getName :: [String] -> String
        getName x = takeWhile (/= ',') . (!! 4) . split ":" . safeHead $ x
        safeHead [] = []
        safeHead (x:_) = x

wrap :: [a] -> [a] -> [a] -> [a]
wrap x y z = x ++ z ++ y
