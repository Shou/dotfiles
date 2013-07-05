
import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.XMonadLog
import System.Taffybar.SimpleClock
import System.Taffybar.Battery
import System.Taffybar.Widgets.PollingGraph
import System.Information.CPU
import System.Taffybar.FSMonitor

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad

import Data.Maybe
import Data.String.Utils (split)

import Graphics.UI.Gtk

import System.IO
import System.Process


-- TODO
-- - Make a queue for the scroll event that counts down when a scroll is
--   triggered and unless another scroll is triggered within the timeout,
--   does whatever action it was supposed to do, otherwise it it combines
--   the actions and only runs `runCommand' once. omgoptimized

data Scroller = Scroller { sc :: ScrollDirection, td :: Int }

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

    -- Current workspace
    (_, Just h, _, _) <- createProcess $ do
        (shell "wmctrl -d | grep '*' | sed 's_\\([0-9]\\+\\).*_\\1_'") { std_out = CreatePipe }
    str <- hGetContents h
    let n = maybe 0 id $ maybeRead str

    -- TMVar for the scroll event
    tmvar <- newTMVarIO n
    running <- newTMVarIO False

    -- Haskell logo
    pixbuf <- pixbufNewFromFileAtScale "/home/hatate/.xmonad/icons/Haskell-logo-white.svg" (-1) 14 True
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
        void . liftIO $ do
            let f = dirToN dir
            atomically $ takeTMVar tmvar >>= return . f >>= putTMVar tmvar
            threadDelay 1000
            n <- atomically $ readTMVar tmvar
            runCommand $ "wmctrl -s " ++ show n

    widgetShowAll ebox
    return (toWidget ebox)
  where
    dirToN ScrollUp = moreZero . pred
    dirToN ScrollDown = succ
    dirToN _ = id
    moreZero n | n < 0 = 0 | otherwise = n

expertClock :: IO Widget
expertClock = do
    clock <- textClockNew Nothing "<span font='Terminus 8' fgcolor='#fcfcfc'>%a %b %_d, %H:%M:%S</span>" 1
    return clock

cpuCallback = do
    (_, systemLoad, totalLoad) <- cpuLoad
    return [ totalLoad, systemLoad ]

main :: IO ()
main = do
    let cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                        , (1, 0, 1, 0.5)
                                                        ]
                                    , graphLabel = Just "cpu"
                                    }
        clock = expertClock
        log = expertLog
        tray = systrayNew
        battery = textBatteryNew "<span face='Terminus' size='8' weight='bold'>%d%%</span>" 15
        cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
        fsm = fsMonitorNew 5.0 ["/dev/sda1"]
    defaultTaffybar defaultTaffybarConfig { startWidgets    = [ log ]
                                          , endWidgets      = [ name, tray, battery ]
                                          , centerWidget    = clock
                                          , barHeight       = 18
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

maybeRead :: Read a => String -> Maybe a
maybeRead x = listToMaybe . fmap fst $ reads x

wrap :: [a] -> [a] -> [a] -> [a]
wrap x y z = x ++ z ++ y
