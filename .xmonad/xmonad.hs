-- ~/.xmonad/xmonad.hs
-- Imports {{{
import           XMonad
-- Prompt
import           XMonad.Prompt
import           XMonad.Prompt.AppendFile       (appendFilePrompt)
import           XMonad.Prompt.RunOrRaise       (runOrRaisePrompt)
import           XMonad.Prompt.Shell            (prompt, safePrompt,
                                                 shellPrompt)

-- Hooks
import           XMonad.Operations

import           System.Exit
import           System.IO

import           XMonad.Util.Run


import           XMonad.Actions.CycleWS
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.Search          (google, promptSearch, scholar,
                                                 selectSearch, wikipedia)
import           XMonad.Actions.WindowGo        (raiseEditor, raiseMaybe,
                                                 runOrRaise)
import XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook

import           Data.Ratio                     ((%))
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders        (noBorders, smartBorders, lessBorders, Ambiguity(OnlyFloat))
import           XMonad.Layout.PerWorkspace     (onWorkspace, onWorkspaces)
import           XMonad.Layout.Reflect          (reflectHoriz)
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Spacing
import           XMonad.Util.Run

import qualified Data.Map                       as M
import qualified XMonad.StackSet                as W
import XMonad.Config.Desktop

--}}}

-- Config {{{
-- Define Terminal
myTerminal      = "gnome-terminal"
-- myTerminal      = "lxterminal"
-- Define modMask
modMask' :: KeyMask
modMask' = mod4Mask
-- Define workspaces
myWorkspaces    = ["1:main","2:web","3:emacs","4:chat","5:music", "6:gimp", "7:misc", "8:eclipse", "9:intellinet"]
-- Dzen/Conky
--myXmonadBar = "dzen2 -x '1440' -y '0' -h '24' -w '640' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
-- option -e 'button3='  needed to prevent right click to kill the status bar
-- https://github.com/robm/dzen/wiki/Events-and-actions
myXmonadBar = "dzen2 -e 'button3=' -x '900' -y '0' -h '24' -w '700' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
myStatusBar = "conky -c /home/fb019397/.xmonad/.conky_dzen2 | dzen2 -e 'button3=' -x '0' -w '900' -h '24' -ta 'l' -bg '#1B1D1E' -fg '#FFFFFF' -y '0'"
myBitmapsDir = "/home/fb019397/.xmonad/dzen2"
--}}}
-- Main {{{
main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "red", "fg", "black", "-xs", "1", "-y", "20"] } urgencyConfig { remindWhen = Every 15 } $ ewmh desktopConfig
      { terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = keys'
      , modMask             = modMask'
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , logHook             = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
      , borderWidth         = 2
      , startupHook         = do
          setWMName "LG3D"
          docksStartupHook
      , handleEventHook    = docksEventHook <+> fullscreenEventHook
}
--}}}


-- Hooks {{{
-- ManageHook {{{
manageHook' :: ManageHook
manageHook' =  composeAll . concat $
      [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
      ,[ className =? "trayer" --> doIgnore]
      , [className    =? c            --> doShift  "1:main"   |   c   <- myDev    ] -- move dev to main
      , [className    =? c            --> doShift  "2:web"    |   c   <- myWebs   ] -- move webs to main
      , [className    =? c            --> doShift  "3:emacs"    |   c   <- myVim    ] -- move webs to main
      , [className    =? c            --> doShift "4:chat"   |   c   <- myChat   ] -- move chat to chat
      , [className    =? c            --> doShift  "5:music"  |   c   <- myMusic  ] -- move music to music
      , [className    =? c            --> doShift  "6:gimp"   |   c   <- myGimp   ] -- move img to div
      , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
      , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
      , [isFullscreen                 --> myDoFullFloat                           ]
--      , [isFullscreen                 --> myDoFullFloat                           ]
      , [manageDocks]]

    where
        -- role     = stringProperty "WM_WINDOW_ROLE"
        name     = stringProperty "WM_NAME"

        -- classnames
        myFloats = ["Smplayer","MPlayer","VirtualBox","Xmessage","XFontSel","Downloads","Nm-connection-editor"]
        myWebs   = ["Firefox","Google-chrome","google-chrome","Chromium", "Chromium-browser"]
        myMusic  = ["Rhythmbox","Spotify","Clementine", "quodlibet", "Quodlibet"]
        myChat   = ["Pidgin","Buddy List", "Psi", "Psi+", "chat", "psi"]
        myGimp   = ["Gimp"]
        myDev    = ["gnome-terminal"]
        myVim    = ["emacs","Emacs","Emacs24","Gvim"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer,Trayer"]

        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = --doF W.focusDown <+>
  doFullFloat
-- }}}
layoutHook'  =  lessBorders OnlyFloat  $ avoidStruts $
                onWorkspaces ["1:main","5:music"] customLayout $
                onWorkspaces ["6:gimp"] gimpLayout $
                onWorkspaces ["4:chat"] imLayout $
                onWorkspaces ["8:eclipse"] customLayout3
                customLayout2

--Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#1B1D1E"
      , ppVisible           =   dzenColor "white" "#1B1D1E"
      , ppHidden            =   dzenColor "white" "#1B1D1E"
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E"
      , ppUrgent            =   dzenColor "black" "red"
      , ppWsSep             =   " "
      , ppSep               =   " |"
      , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }

-- Layout
customLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| simpleFloat
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

customLayout2 = avoidStruts $ Full ||| tiled ||| Mirror tiled ||| simpleFloat
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

customLayout3 = avoidStruts $ simpleFloat ||| Full ||| tiled ||| Mirror tiled
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

gimpLayout  = avoidStruts $ withIM (0.11) (Role "gimp-toolbox") $
              reflectHoriz $
              withIM (0.15) (Role "gimp-dock") Full

imLayout    = avoidStruts $  withIM (1%5) (Role "buddy_list") Grid
--}}}
-- Theme {{{
-- Color names are easier to remember:
colorOrange         = "#FD971F"
colorDarkGray       = "#1B1D1E"
colorPink           = "#F92672"
colorGreen          = "#A6E22E"
colorBlue           = "#66D9EF"
colorYellow         = "#E6DB74"
colorWhite          = "#CCCCC6"
colorNormalBorder   = "#CCCCC6"
colorFocusedBorder  = "#fd971f"


barFont  = "terminus"
barXFont = "inconsolata:size=12"
xftFont = "xft: inconsolata-14"
--}}}

-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 14
                    , historyFilter         = deleteConsecutive
                    }

-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 22
                }

--inclasses

-- }}}
-- Key mapping {{{
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_p        ), runOrRaisePrompt largeXPConfig)
--     ((modMask,               xK_p     ), spawn "dmenu_run") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask .|. shiftMask,      xK_Return   ), spawn $ XMonad.terminal conf)
    , ((modMask,                    xK_F2       ), spawn "gmrun")
    , ((modMask .|. shiftMask,      xK_c        ), kill)
    , ((modMask .|. shiftMask,      xK_l        ), spawn "qdbus org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock; xset dpms force suspend")
    , ((modMask .|. shiftMask,      xK_t        ), runOrRaise "thunderbird" (foldl (\b x -> b <||> (className =? x)) (return False) ["Thunderbird", "Main"]))
    , ((modMask .|. shiftMask,      xK_a        ), raiseMaybe (spawn "thunderbird -addressbook") (foldl (\b x -> b <||> (className =? x)) (return False) ["Thunderbird", "Main"]))
    -- Programs
    , ((0,                          xK_Print    ), spawn "scrot -e 'mv $f ~/screenshots/'")
--    , ((modMask,		            xK_o        ), spawn "chromium-browser")
    , ((modMask,                    xK_m        ), spawn "nautilus --no-desktop --browser")
    , ((modMask,                    xK_f        ), runOrRaise "firefox" (className =? "Firefox"))
    , ((modMask,              xK_o), spawn "emacsclient -nc")
    , ((modMask .|. shiftMask,xK_o), prompt "emacsclient -nc" greenXPConfig)
    , ((modMask,              xK_g), promptSearch greenXPConfig google)
    , ((modMask .|. shiftMask,xK_g), selectSearch google)
--    , ((modMask,              xK_w), promptSearch greenXPConfig wikipedia)
--    , ((modMask .|. shiftMask,xK_w), selectSearch wikipedia)

    --    , ((modMask,                    xK_e        ), spawnPipe "emacsclient-nc")
    -- Media Keys
    , ((0,                          0x1008ff12  ), spawn "amixer -D pulse sset Master toggle")        -- XF86AudioMute
    , ((0,                          0x1008ff11  ), spawn "amixer -D pulse sset Master 5%-")   -- XF86AudioLowerVolume
    , ((0,                          0x1008ff13  ), spawn "amixer -D pulse sset Master 5%+")   -- XF86AudioRaiseVolume
    , ((controlMask,                          0x1008ff14  ), spawn "quodlibet --toggle-window")
    , ((controlMask,                          0x1008ff17  ), spawn "quodlibet --seek=+1")
    , ((controlMask,                          0x1008ff16  ), spawn "quodlibet --seek=-1")
    , ((modMask .|. shiftMask,            xK_f  ), spawn "~/scripts/arandr/toggle.sh")
    -- , ((0,                          0x1008ff14  ), spawn "rhythmbox-client --play-pause")
    -- , ((0,                          0x1008ff17  ), spawn "rhythmbox-client --next")
    -- , ((0,                          0x1008ff16  ), spawn "rhythmbox-client --previous")

    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf)          -- reset layout on current desktop to default
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts)
    , ((modMask,                    xK_n        ), refresh)
    , ((modMask,                    xK_Tab      ), windows W.focusDown)                         -- move focus to next window
    , ((modMask.|. shiftMask,       xK_Tab      ), windows W.focusUp)                         -- move focus to next window
    , ((modMask,                    xK_j        ), windows W.focusDown)
    , ((modMask,                    xK_k        ), windows W.focusUp  )
    , ((modMask .|. shiftMask,  xK_j        ), windows W.swapDown)                          -- swap the focused window with the next window
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp)                            -- swap the focused window with the previous window
    , ((modMask,                    xK_Return   ), windows W.swapMaster)
    , ((modMask,                    xK_t        ), withFocused $ windows . W.sink)              -- Push window back into tiling
    , ((modMask,                    xK_h        ), sendMessage Shrink)                          -- %! Shrink a master area
    , ((modMask,                    xK_l        ), sendMessage Expand)                          -- %! Expand a master area
    , ((modMask,                    xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                    xK_period   ), sendMessage (IncMasterN (-1)))


    -- workspaces
    , ((modMask .|. controlMask,   xK_Right     ), nextWS)
    , ((modMask .|. controlMask .|. shiftMask,     xK_Right     ), shiftToNext >> nextWS)
    , ((modMask .|. controlMask,   xK_Left      ), prevWS)
    , ((modMask .|. controlMask .|. shiftMask,     xK_Left      ), shiftToPrev >> prevWS)

    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask,                    xK_q        ), spawn myRestart)
    -- change keyboard layouts
    , ((modMask .|. controlMask, xK_space), spawn "/home/fb019397/scripts/keyboard/keymap.sh")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [1, 2, 0]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

--}}}
-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap

-- Kill zombie dzens before normal xmonad restart
myRestart :: String
myRestart = "for pid in `pgrep dzen2`; do kill -9 $pid; done && xmonad --recompile && xmonad --restart"
