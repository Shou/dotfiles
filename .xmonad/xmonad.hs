-- ~/.xmonad/xmonad.hs
-- Imports {{{
import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Shell
-- Hooks
import XMonad.Operations
 
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (void)
import System.IO
import System.Exit
 
import XMonad.Util.Run
 
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Actions.FloatKeys
 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import System.Taffybar.XMonadLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
 
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.Spacing
 
import Data.Ratio ((%))
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M
 
import DBus.Client.Simple
--}}}
 
-- Config {{{
-- Define Terminal
myTerminal      = "urxvt"
-- Define modMask
modMask' :: KeyMask
modMask' = mod4Mask

-- Workspaces.
myWorkspaces = do
    x <- [1 .. 16]
    return $ show x
--myWorkspaces    = ["一","二","三","四","五", "六", "七","八","九", "十"]

--}}}
-- Main {{{
main = do
    bar <- spawn "taffybar"
    client <- connectSession
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig
      { terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = keys'
      , mouseBindings       = mouse
      , modMask             = modMask'
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , logHook             = dbusLogWithPP client myTaffyPP -- >> fadeInactiveLogHook 0xdddddddd
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
      , borderWidth         = 1
      , focusFollowsMouse   = True
      }
--}}}
 
 
-- Hooks {{{
-- ManageHook {{{
manageHook' :: ManageHook
manageHook' = manageSpawn <+> manageDocks <+> (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    --, [className    =? c            --> doShift  "web"      |   c   <- myWebs   ] -- move webs to ws2
    , [className    =? c            --> doShift  "6"        |   c   <- myGimp   ] -- move img to ws5
    , [className    =? c            --> doFloat             |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doFloat             |   n   <- myNames  ] -- float my names
    , [className    =? c            --> myDoFullFloat       |   c   <- myFullClasses ] -- full float my names
    , [isFullscreen                 --> doFullFloat]
    ]) 
 
    where
 
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
 
        -- classnames
        myFloats  = [ "Smplayer2"
                    , "Smplayer"
                    , "mplayer2"
                    , "VirtualBox"
                    , "Xmessage"
                    , "XFontSel"
                    , "Download"
                    , "Nm-connection-editor"
                    , "Volumeicon"
                    , "Dialog"
                    , "net-minecraft-LauncherFrame"
                    ]
        myWebs    = ["Firefox","Google-chrome","Chromium", "Chromium-browser"]
        myGimp    = ["Gimp"]
 
        -- resources
        myIgnores = [ "desktop"
                    , "desktop_window"
                    , "notify-osd"
                    , "stalonetray"
                    , "trayer"
                    ]
 
        -- names
        myNames   = [ "bashrun"
                    , "Google Chrome Options"
                    , "Chromium Options"
                    , "volumeicon"
                    , "Minecraft Launcher"
                    ]

        -- full class names
        myFullClasses = [ "comix"
                        , "Comix"
                        , "eog"
                        , "Eog"
                        ]
 
-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
-- }}}
layoutHook'  =  onWorkspaces ["1","2"] fullLayout $ 
                onWorkspaces ["3"] progLayout $ 
                onWorkspaces ["4"] generalLayout $ 
                onWorkspaces ["6"] gimpLayout $ 
                normalLayout
 
-- Layouts
normalLayout = avoidStruts $ tiled
                         ||| reflectHoriz tiled
                         ||| reflectHoriz Grid
                         ||| horizTiled
                         ||| noBorders Full
                         ||| gimpFull
 
generalLayout = avoidStruts $ reflectHoriz Grid
                          ||| horizTiled
                          ||| noBorders Full
                          ||| gimpFull
                          ||| tiled
                          ||| reflectHoriz tiled
 
fullLayout = avoidStruts $ noBorders Full
                       ||| gimpFull
                       ||| tiled
                       ||| reflectHoriz tiled
                       ||| reflectHoriz Grid
                       ||| horizTiled
 
progLayout = avoidStruts $ horizTiled
                       ||| noBorders Full
                       ||| gimpFull
                       ||| tiled
                       ||| reflectHoriz tiled
                       ||| reflectHoriz Grid
 
gimpLayout = avoidStruts $ gimpFull
                       ||| tiled
                       ||| reflectHoriz tiled
                       ||| reflectHoriz Grid
                       ||| horizTiled
                       ||| noBorders Full
 
tiled = ResizableTall 1 (2/100) (1/2) []
horizTiled = Mirror $ ResizableTall 1 (16/100) (0.8) []
gimpFull = withIM 0.11 (Role "gimp-toolbox") $
           reflectHoriz $
           withIM 0.15 (Role "gimp-dock") Full
 
--}}}
-- Theme {{{
 
myTaffyPP = defaultPP   { ppCurrent         = \_ -> tfont "Terminus 8, Mona Bold 8" $ tfgcolor currentTheme "■"
                        , ppVisible         = \_ -> tfont "Terminus 8, Mona 8" $ tfgcolor currentTheme "▣"
                        , ppHidden          = \_ -> tfont "Terminus 8, Mona 8" $ tfgcolor currentBright "□"
                        , ppHiddenNoWindows = \_ -> []
                        , ppUrgent          = \_ -> tfont "Terminus 8, Mona 8" . tbold $ tfgcolor "#ff0090" "▣"
                        , ppTitle           = tfont "Terminus 8" . cutoff 64
                        , ppSep             = " <b>::</b> "
                        , ppWsSep           = " "
                        , ppLayout          = iconify
                        }
  where wrap x y z = x ++ z ++ y
        iconify "ResizableTall"             = "◧"
        iconify "ReflectX ResizableTall"    = "◨"
        iconify "ReflectX Grid"             = "▦"
        iconify "Mirror ResizableTall"      = "▬"
        iconify "Full"                      = "■"
        iconify "IM ReflectX IM Full"       = "◫"
        iconify x                           = x
        cutoff n xs | length xs <= n = xs | otherwise = take n xs ++ "…"
        tfgcolor x y = "<span fgcolor='" ++ x ++ "'>" ++ y ++ "</span>"
        tbgcolor x y = "<span bgcolor='" ++ x ++ "'>" ++ y ++ "</span>"
        tfont x y = "<span font='" ++ x ++ "'>" ++ y ++ "</span>"
        tbold y = "<b>" ++ y ++ "</b>"
 
colorNormalBorder   = "#2d2d2d"
colorFocusedBorder  = currentTheme
 
currentTheme = cHatate
currentBright = cDark

cNitori = "#66ccff"
cHatate = "#7e40a5"
cShou = "#960050"
cSuwako = "#19dceb"

weHackerNow = "#66aa11"
cLight = "#f0f0f0"
cDark = "#101010"
 
barFont     = "xft:terminus:size=8"
panelFont   = "-xos4-terminus-medium-r-normal--12-120-72-72-c-60-iso8859-1"
--}}}
 
-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = "#101010"
                    , fgColor               = "#d0d0d0"
                    , bgHLight              = "#101010"
                    , fgHLight              = currentTheme
                    , promptBorderWidth     = 0
                    , height                = 16
                    , historyFilter         = deleteConsecutive
                    }
 
myGSConfig = defaultGSConfig {
      gs_colorizer = current
    , gs_font = barFont
    , gs_bordercolor = currentBright
    }
  where
    current = case cHatate of
        cHatate -> c (0,0,0) (25,25,25) (55,12,90) (205,205,205) (250,250,250)
        cNitori -> c (255,255,255) (205,205,205) (106,204,255) (109,172,132) (255,255,255)
        cShou -> c (250,250,250) (249,225,189) (124,1,1) (211,110,7) (250,250,250)
        cSuwako -> c (0,0,0) (25,25,25) (1,192,165) (70,33,66) (255,255,255)
        _ -> c (0,0,0) (25,25,25) (237,237,237) (127,127,127) (237,237,237)
    c = colorRangeFromClassName
 
-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig

-- }}}
-- Key mapping {{{
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_p        ), shellPrompt largeXPConfig)
    , ((modMask .|. shiftMask,      xK_Return   ), spawnHere $ XMonad.terminal conf)
    , ((modMask,                    xK_F2       ), spawnHere "gmrun")
    , ((modMask,                    xK_c        ), kill)
    , ((controlMask .|. mod1Mask,   xK_l        ), spawnHere "gnome-screensaver-command -l")
 
    -- Programs
    , ((controlMask .|. mod1Mask,   xK_c        ), spawnOn "2" "firefox")
    , ((controlMask .|. mod1Mask,   xK_h        ), spawnHere "nautilus --no-desktop")
    , ((controlMask .|. shiftMask,  xK_Escape   ), spawnHere "gnome-system-monitor")
 
    -- Screenshots
    , ((0,                          xK_Print    ), submap . M.fromList $
        [ ((0,                          xK_Print    ), spawnHere "scrot -q0 'scrot-$wx$h-%s.png' -e 'mv $f /home/hatate/Desktop/'")
        , ((0,                          xK_w        ), spawnHere "scrot -q0 -u 'scrot-$wx$h-%s.png' -e 'mv $f /home/hatate/Desktop/'")
        , ((controlMask,                xK_Print    ), spawnHere "scrot -q95 'scrot-$wx$h-%s.jpg' -e 'mv $f /home/hatate/Desktop/'")
        , ((controlMask,                xK_w        ), spawnHere "scrot -q95 -u 'scrot-$wx$h-%s.jpg' -e 'mv $f /home/hatate/Desktop/'")
        , ((0,                          xK_t        ), spawnHere "scrot -d 5 -q0 'scrot-$wx$h-%s.png' -e 'mv $f /home/hatate/Desktop/'")
        , ((controlMask,                xK_t        ), spawnHere "scrot -d 5 -q95 'scrot-$wx$h-%s.jpg' -e 'mv $f /home/hatate/Desktop/'")
        , ((0,                          xK_c        ), spawnHere "gnome-screenshot -c")
        , ((0,                          xK_i        ), spawnHere "gnome-screenshot -i")
        , ((0,                          xK_a        ), spawnHere "scrot -s -q0 'scrot-$wx$h-%s.png' -e 'mv $f /home/hatate/Desktop/'")
        ])
 
    -- Media Keys
    , ((modMask,                    xK_m        ), submap . M.fromList $
        [ ((modMask,                    xK_l        ), spawnHere "mpc next")
        , ((modMask,                    xK_h        ), spawnHere "mpc prev")
        , ((modMask,                    xK_j        ), spawnHere "mpc volume -2")
        , ((modMask,                    xK_k        ), spawnHere "mpc volume +2")
        , ((modMask,                    xK_z        ), spawnHere "mpc random")
        , ((modMask,                    xK_r        ), spawnHere "mpc repeat")
        , ((modMask,                    xK_y        ), spawnHere "mpc single")
        , ((modMask,                    xK_u        ), spawnHere "mpc update")
        , ((modMask,                    xK_p        ), spawnHere "mpc toggle")
        , ((modMask,                    xK_d        ), spawnHere "echo 0 | mpc del")
        ])
 
    -- Volume keys
    , ((modMask,                    xK_v        ), submap . M.fromList $
        [ ((modMask,                    xK_m        ), spawnHere $ pacmd "0")
        , ((modMask,                    xK_1        ), spawnHere $ pacmd "6553")
        , ((modMask,                    xK_2        ), spawnHere $ pacmd "13107")
        , ((modMask,                    xK_3        ), spawnHere $ pacmd "19660")
        , ((modMask,                    xK_4        ), spawnHere $ pacmd "26214")
        , ((modMask,                    xK_5        ), spawnHere $ pacmd "32768")
        , ((modMask,                    xK_6        ), spawnHere $ pacmd "39321")
        , ((modMask,                    xK_7        ), spawnHere $ pacmd "45875")
        , ((modMask,                    xK_8        ), spawnHere $ pacmd "52428")
        , ((modMask,                    xK_9        ), spawnHere $ pacmd "58982")
        , ((modMask,                    xK_0        ), spawnHere $ pacmd "65536")
        ])

    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf)          -- reset layout on current desktop to default
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts)
    , ((modMask,                    xK_n        ), refresh)
    , ((modMask,                    xK_Tab      ), windows W.focusDown)                         -- move focus to next window
    , ((modMask,                    xK_j        ), windows W.focusDown)
    , ((modMask,                    xK_k        ), windows W.focusUp  )
    , ((modMask .|. shiftMask,      xK_j        ), windows W.swapDown)                          -- swap the focused window with the next window
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp)                            -- swap the focused window with the previous window
    , ((modMask,                    xK_Return   ), windows W.swapMaster)
    , ((modMask,                    xK_t        ), withFocused $ windows . W.sink)              -- Push window back into tiling
    , ((modMask,                    xK_h        ), sendMessage Shrink)                          -- %! Shrink a master area
    , ((modMask,                    xK_l        ), sendMessage Expand)                          -- %! Expand a master area
    , ((modMask,                    xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                    xK_period   ), sendMessage (IncMasterN (-1)))
    , ((modMask,                    xK_a        ), focusUrgent)
    , ((modMask,                    xK_g        ), goToSelected myGSConfig)
 
    -- Window management
    , ((modMask,                    xK_f        ), submap . M.fromList $
        [ ((modMask,                    xK_c        ), withFocused $ keysMoveWindowTo (1366 `div` 2, 768 `div` 2) (1%2, 1%2))  -- center
        , ((modMask,                    xK_l        ), withFocused $ keysMoveWindowTo (1366 - 2, 768 `div` 2) (1, 1%2))        -- center right
        , ((modMask .|. shiftMask,      xK_l        ), withFocused $ keysMoveWindowTo (1366 - 2, 0) (1, 0))                     -- top right
        , ((modMask .|. controlMask,    xK_l        ), withFocused $ keysMoveWindowTo (1366 - 2, 768 - 2) (1, 1))              -- bottom right
        , ((modMask,                    xK_h        ), withFocused $ keysMoveWindowTo (1, 768 `div` 2) (0, 1%2))               -- center left
        , ((modMask .|. shiftMask,      xK_h        ), withFocused $ keysMoveWindowTo (0, 0) (0, 0))                            -- top left
        , ((modMask .|. controlMask,    xK_h        ), withFocused $ keysMoveWindowTo (0, 768 - 2) (0, 1))                     -- bottom left
        , ((modMask,                    xK_j        ), withFocused $ keysMoveWindowTo (1366 `div` 2, 768) (1%2, 1))            -- center bottom
        , ((modMask,                    xK_k        ), withFocused $ keysMoveWindowTo (1366 `div` 2, 0) (1%2, 0))               -- center top
        , ((modMask,                    xK_o        ), withFocused $ keysMoveWindowTo (1366 `div` 2, 768) (0, 0))              -- hide off-screen
        ])
 
    -- workspaces
    , ((modMask,                        xK_d        ), nextWS)
    , ((modMask .|. shiftMask,          xK_d        ), shiftToNext >> nextWS)
    , ((modMask,                        xK_s        ), prevWS)
    , ((modMask .|. shiftMask,          xK_s        ), shiftToPrev >> prevWS)
 
    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask,                    xK_q        ), spawnHere "/home/suwako/.xmonad/scripts/kill-taffybar && xmonad --recompile && xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ [xK_1 .. xK_9] ++ [xK_0]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- Numpad numbers as well
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ [ xK_KP_End          -- 1
                                                   , xK_KP_Down         -- 2
                                                   , xK_KP_Page_Down    -- 3
                                                   , xK_KP_Left         -- 4
                                                   , xK_KP_Begin        -- 5
                                                   , xK_KP_Right        -- 6
                                                   , xK_KP_Home         -- 7
                                                   , xK_KP_Up           -- 8
                                                   , xK_KP_Page_Up      -- 9
                                                   , xK_KP_Insert ]     -- 0
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
mouse conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    button1), (\w -> focus w >> mouseMoveWindow w >> snapMagicMove (Just 200) (Just 200) w))
    , ((modMask,                    button3), (\w -> focus w >> mouseResizeWindow w >> snapMagicResize [R,D] (Just 200) (Just 200) w))
    , ((modMask,                    button2), (\w -> focus w))
    ,((0,                           button8), \_ -> spawnHere "mpc next")
    ,((0,                           button9), \_ -> spawnHere "mpc prev")
    ]
  where button8 = 8 :: Button
        button9 = 9 :: Button
--}}}

-- {{{ Helper functions

pacmd :: String -> String
pacmd v = "pacmd set-sink-volume $(pacmd list-sinks | grep -Z index | sed -e s/\\ \\ \\ \\ index:\\ //) " ++ v

-- | what the hell am I even doing
forkX :: X a -> X ThreadId
forkX m = do
    r <- ask
    st <- get
    liftIO . forkIO . void $ runX r st m

-- }}}
-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
