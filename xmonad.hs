-- ~/.xmonad/xmonad.hs
-- Imports {{{
import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
-- Hooks
import XMonad.Operations
 
import System.IO
import System.Exit
 
import XMonad.Util.Run
 
 
import XMonad.Actions.CycleWS
 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
 
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.Gaps
 
import Data.Ratio ((%))
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M
 
--}}}
 
-- Config {{{
myTerminal      = "xterm"
myStartScript = "~/.xmonad/.start"

{- set to mod4Mask for super/windows key, 1 is alt -}
modMask' :: KeyMask
modMask' = mod1Mask

-- myWorkspaces    = ["1: main","2:conf","3:web","4:xterm","5:music", "6:xterm", "7", "8", "9"]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

{- Dzen/Conky status bar -}

{- global definitions for status bar background and foreground -}
myBarBg = "#6A7F8D"
myBarFg = "#384E53"

{- Local installations, change you your install location -}
dzenLoc = "~/usr/local/bin/dzen2"
conkyLoc = "~/snacks/bin/conky"
conkyConfig = "~/.xmonad/.conky_dzen"

{- commands to create the bars -}
myXmonadBar = dzenLoc ++ " -x '0' -y '0' -h '18' -w '1420' -ta 'l' -fg '" ++ myBarBg ++ "' -bg '" ++ myBarBg ++ "'"
myStatusBar = conkyLoc ++ " -c " ++ conkyConfig ++ " | " ++ dzenLoc ++ " -x '1420' -w '500' -h '18' -ta 'r' -bg'" ++ myBarBg ++ "' -fg '" ++ myBarFg ++ "' -y '0'"

myBitmapsDir = "/uio/hume/student-u56/espenaj/.xmonad/dzen2"

--}}}
-- Main {{{
main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    spawn myStartScript

    xmonad $ defaultConfig
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
      }
--}}}
 
-- Hooks {{{
-- ManageHook {{{
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    ]) 
 
    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
 
        myFloats  = ["VirtualBox","Xmessage","XFontSel","Downloads","Nm-connection-editor"]
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]
-- }}}

{- LayoutHook 
 - Enable gapping to alinge border perfect to top bar 
 - I like to have one workspace floating, just remove first line if you don't -}
layoutHook'  =  
                onWorkspace "4" simpleFloat $  
                gaps [(U,19), (R,1), (L,1), (D,1)] $ Tall 1 (3/100) (1/2) ||| Full  
 
{- Bar
 - take 1 to print only first char of layout, take 0 to print none -}
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
          ppTitle             =   take 0 
        , ppHiddenNoWindows   =   dzenColor "black" "#8395A0" . pad 
        , ppHidden            =   dzenColor "black" "#9CAAB3" . pad 
        , ppCurrent           =   dzenColor "white" "#146CA6" . pad
        , ppVisible           =   dzenColor "black" "#999999" . pad
        , ppWsSep             =   ""
        , ppSep               =   " "
        , ppLayout            =   dzenColor "#2b4f98" "#6A7F8D" . take 1 
        , ppOutput            =   hPutStrLn h
    }
 
--}}}

-- Theme {{{
colorNormalBorder   = "#082B42"
colorFocusedBorder  = "#146CA6"
 
{- not sure what these does -} 
barFont  = "Caladea"
barXFont = "inconsolata:size=12"
xftFont = "xft: inconsolata-14"
--}}}
 
-- }}}
-- Key mapping {{{
{- modMask = alt, cant be changed in top of config -}
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_Return   ), spawn $ XMonad.terminal conf)
    , ((modMask,                    xK_F2       ), spawn "gmrun")
    , ((modMask .|. shiftMask,      xK_q        ), kill)
    , ((modMask .|. shiftMask,      xK_l        ), spawn "xscreensaver-command -lock")

    -- Programs
    , ((0,                          xK_Print    ), spawn "scrot -e 'mv $f ~/screenshots/'")
    , ((modMask,		            xK_o        ), spawn "chromium-browser")
    , ((modMask,                    xK_m        ), spawn "nautilus --no-desktop --browser")
    , ((modMask,                    xK_d        ), spawn "dmenu_run")

    -- Media Keys
    , ((0,                          0x1008ff12  ), spawn "amixer -q sset Headphone toggle")-- XF86AudioMute
    , ((0,                          0x1008ff11  ), spawn "amixer -q sset Headphone 5%-")   -- XF86AudioLowerVolume
    , ((0,                          0x1008ff13  ), spawn "amixer -q sset Headphone 5%+")   -- XF86AudioRaiseVolume
    , ((0,                          0x1008ff14  ), spawn "rhythmbox-client --play-pause")
    , ((0,                          0x1008ff17  ), spawn "rhythmbox-client --next")
    , ((0,                          0x1008ff16  ), spawn "rhythmbox-client --previous")
 
    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf) -- reset layout on current desktop to default
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts)
    , ((modMask,                    xK_n        ), refresh)
    , ((modMask,                    xK_Tab      ), windows W.focusDown)                -- move focus to next window
    , ((modMask,                    xK_j        ), windows W.focusDown)
    , ((modMask,                    xK_k        ), windows W.focusUp  )
    , ((modMask .|. shiftMask,      xK_j        ), windows W.swapDown)                 -- swap the focused window with the next window
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp)                   -- swap the focused window with the previous window
    , ((modMask .|. shiftMask,      xK_Return   ), windows W.swapMaster)
    , ((modMask,                    xK_t        ), withFocused $ windows . W.sink)     -- Push window back into tiling
    , ((modMask,                    xK_h        ), sendMessage Shrink)                 -- %! Shrink a master area
    , ((modMask,                    xK_l        ), sendMessage Expand)                 -- %! Expand a master area
    , ((modMask,                    xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                    xK_period   ), sendMessage (IncMasterN (-1)))

    -- gaps
    , ((modMask .|. controlMask, xK_g), sendMessage $ ToggleGaps)  -- toggle all gaps
    , ((modMask .|. controlMask, xK_t), sendMessage $ ToggleGap U) -- toggle the top gap
    , ((modMask .|. controlMask, xK_w), sendMessage $ IncGap 1 U)  -- increment the top gap
    , ((modMask .|. controlMask, xK_q), sendMessage $ DecGap 1 U)  -- decrement the top gap
 
    -- workspaces
    , ((modMask .|. controlMask,   xK_Right     ), nextWS)
    , ((modMask .|. shiftMask,     xK_Right     ), shiftToNext)
    , ((modMask .|. controlMask,   xK_Left      ), prevWS)
    , ((modMask .|. shiftMask,     xK_Left      ), shiftToPrev)
 
    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_y        ), io (exitWith ExitSuccess))
    , ((modMask,                    xK_q        ), spawn "killall conky dzen2 && /home/my_user/.cabal/bin/xmonad --recompile && /home/my_user/.cabal/bin/xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
--}}}
-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
