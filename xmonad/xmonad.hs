
import XMonad

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ratio

import System.Environment (getEnv)
import System.Exit
import System.IO
import System.Posix.Unistd

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Graphics.X11.Xlib
import IO (Handle, hPutStrLn)

-- utils
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers (maildirNew,logCmd,dzenColorL,wrapL,shortenL)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

-- actions
import XMonad.Actions.CycleWS

-- layouts
import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints (layoutHintsWithPlacement)
import XMonad.Layout.Maximize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup,ewmh)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
main = do
   host <- fmap nodeName getSystemID
   homedir <- getEnv "HOME"
   let dzenbar = (if host == "fireball" then
                    myStatusBar leftBarWidthL
                  else
                    myStatusBar leftBarWidth)
   let dzenobar = (if host == "fireball" then
                     myOtherBar rightBarWidthL leftBarWidthL
                   else
                     myOtherBar rightBarWidth leftBarWidth)

   d <- spawnPipe dzenbar
   spawnPipe dzenobar

   xmonad $ ewmh $ withUrgencyHook NoUrgencyHook $ defaultConfig
       { terminal           = myTerminal
       , borderWidth        = myBorderWidth
       , workspaces         = myWorkspaces
       , normalBorderColor  = myNormalBorderColor
       , focusedBorderColor = myFocusedBorderColor
       , modMask            = myModMask
       , keys               = myKeys
       , manageHook         = myManageHook
       , layoutHook         = myLayout
       , logHook            = myLogHook d (homedir ++ "/.dzen/bitmaps")
       -- , handleEventHook    = mappend myEventHook
       , startupHook        = myStartupHook
       }

------------------------------------------------------------------------
-- Simple stuff

myTerminal          = "urxvtc"
myWorkspaces        = ["main","mail","web","code"] ++ map show [5..9]
myBorderWidth       = 3

-- mod1Mask ("left alt"). mod3Mask ("right alt"). mod4Mask ("windows").
myModMask           = mod4Mask

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = colorFG
myFocusedBorderColor = colorFG4

-- aur/dzen2-svn required for xft fonts
barFont  = "-*-terminus-*-r-normal-*-*-140-*-*-*-*-iso8859-*"
-- barFont  = "terminus"
barXFont = "inconsolata:size=10:bold"
xftFont  = "xft: inconsolata-10"

-- <colors>
-- colorBlack = "#000000"
-- colorBlackAlt = "#040404"
-- colorBlackAlt2 = "#161616"
-- colorGrayAlt = "#282828"
-- colorGrayAlt2 = "#2A2A2A"
-- colorGray = "#606060"
-- colorWhite = "#cfbfad"
-- colorWhiteAlt = "#8c8b8e"
-- colorDarkWhite = "#444444"
-- colorCream = "#a9a6af"
-- colorDarkCream = "#5f656b"
-- colorMagenta = "#a488d9"
-- colorMagentaAlt = "#7965ac"
-- colorDarkMagenta = "#8e82a2"
-- colorBlue = "#98a7b6"
-- colorBlueAlt = "#598691"
-- colorDarkBlue = "#464a4a"
-- colorBlue = "#0066FF"

-- colorOrange          = "#ff7701"
-- colorDarkGray        = "#171717"
-- colorPink            = "#e3008d"
-- colorGreen           = "#00aa4a"
-- colorBlue            = "#008dd5"
-- colorYellow          = "#fee100"
-- colorWhite           = "#cfbfad"

-- colorNormalBorder    = "#1c2636"
-- colorFocusedBorder   = "#2797d8"

-- background/foreground and various levels of emphasis
colorBG              = "#303030"
colorFG              = "#606060"
colorFG2             = "#909090"
-- colorFG3             = "#c4df90"
colorFG4             = "#cc896d" -- limey
colorFG5             = "#c4df90" -- peachy
colorFG6             = "#ffffba" -- yellowy

barHeight            = "20"
leftBarWidth         = "700"
rightBarWidth        = "640"

leftBarWidthL        = "970"
rightBarWidthL       = "870"

-- Status bar
-- myStatusBar= "dzen2 -bg '#222222' -fg '#FFFFFF' -h 14 -w 580 -sa c -e '' -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -ta l"
-- sets up dzen options for two bars displayed as one spanning my 1920 px wide monitor
myStatusBar w = "dzen2 -p -ta l -x 0 -y 0 -fn '" ++ barXFont ++ "'" ++
                " -w " ++ w ++ " -h " ++ barHeight ++
                " -fg '" ++ colorFG ++ "' -bg '" ++ colorBG ++ "'" ++
                " -e 'onexit=ungrabmouse'"

-- myOtherBar  = "conky -c ~/.xmonad/dzen_conkyrc |" ++
myOtherBar w x = "~/.dzen/status.sh |" ++
                 " dzen2 -p -ta l -fn '" ++ barXFont ++ "'" ++
                 " -x " ++ x ++ " -y 0" ++
                 " -w " ++ w ++ " -h " ++ barHeight ++
                 " -fg '" ++ colorFG ++ "' -bg '" ++ colorBG ++ "'" ++
                 " -e 'onexit=ungrabmouse'"

myDmenuBar  = "dmenu_run -i -p '>' -nb '" ++ colorBG ++ "' -nf '" ++ colorFG2 ++
              "' -sb '" ++ colorFG5 ++ "' -sf '" ++ colorFG2 ++ "' -fn '" ++ barFont ++ "'"

------------------------------------------------------------------------
-- Layouts:

-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--  spiral (6/7) |||
--
myLayout = avoidStruts $ onWorkspace "9" simplestFloat $ layouts
  where
    layouts = smartBorders tiled ||| spaced ||| smartBorders (Mirror tiled) |||
              full ||| grid ||| simplestFloat
    spaced = named "Spacing" $ maximize $ hinted $ spacing 6 $ ResizableTall 1 (2/100) (1/2) []
    tiled  = named "Tiled" $ maximize $ hinted $ ResizableTall 1 (2/100) (1/2) []
    grid   = named "Grid" $ maximize $ hinted $ Grid
    full   = named "Full" $ noBorders Full

    hinted l = layoutHintsWithPlacement (0,0) l

     -- default tiling algorithm partitions the screen into two panes
     --tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane: nmaster = 1
     -- Default proportion of screen occupied by master pane: ratio = 1/2
     -- Percent of screen to increment by when resizing panes: delta = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = (composeAll . concat $
  [ [resource  =? r  --> doIgnore         |  r <- myIgnores] -- ignore
  , [className =? c  --> doShift "web"    |  c <- myWebs   ] -- webs
  , [className =? c  --> doShift "mail"   |  c <- myMails  ] -- mails, chats
  -- , [title     =? t  --> doShift "mail"   |  t <- myMails  ] -- mails, chats
  , [className =? c  --> doFloat          |  c <- myFloats ] -- floats
  , [className =? c  --> doCenterFloat    |  c <- myCFloats] -- centered floats
  , [title     =? t  --> doFloat          |  t <- myOtherFloats]
  , [name      =? n  --> doFloat          |  n <- myNames  ] -- names
  , [name      =? n  --> doCenterFloat    |  n <- myCNames ] -- centered names
  , [isFullscreen    --> myDoFullFloat                        ]
  , [(className =? "Firefox" <&&> resource =? "Dialog") --> doCenterFloat]
  ]) <+> manageTypes <+> manageDocks <+> manageScratchPads

  where

    role = stringProperty "WM_WINDOW_ROLE"
    name = stringProperty "WM_NAME"

    myMails       = ["Thunderbird","mutt","irssi","Gajim.py"]
    myFloats      = ["MPlayer","vlc","MediaPlayer","VLC media player"] ++ -- media players
                    ["feh","GIMP","gimp","Gimp","gimp-2.6","Gpicview"] ++ -- image viewers
                    ["Zenity","file_properties","Ediff","Idl","Sonata", "xine"]
    myCFloats     = ["Xmessage","Save As...","XFontSel","gmrun","Galculator"]
    myOtherFloats = ["Bibliothèque Multimédia","VLC media player"] ++
                    ["Bookmarks","Downloads","Add-ons"] ++
                    ["Téléchargements","Préférences de Firefox"] ++
                    ["Open...","Enregistrer sous...","Ouvrir","Delete"]
    myWebs        = ["Navigator","Shiretoko","Firefox"]     ++ -- firefox
                    ["Uzbl","uzbl","Uzbl-core","uzbl-core"] ++ -- uzbl
                    ["Google-chrome","Chromium"]               -- chrom(e|ium)
    myIgnores     = ["desktop","desktop_window","stalonetray","Conky"]
    myNames       = ["Google Chrome Options","Chromium Options"]
    myCNames      = ["bashrun"]

    -- a trick for fullscreen but stil allow focusing of other WSs
    myDoFullFloat :: ManageHook
    myDoFullFloat = doF W.focusDown <+> doFullFloat

    -- modified version of manageDocks
    manageTypes :: ManageHook
    manageTypes = checkType --> doCenterFloat

      where

        checkType :: Query Bool
        checkType = ask >>= \w -> liftX $ do
          m   <- getAtom    "_NET_WM_WINDOW_TYPE_MENU"
          d   <- getAtom    "_NET_WM_WINDOW_TYPE_DIALOG"
          u   <- getAtom    "_NET_WM_WINDOW_TYPE_UTILITY"
          mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w

          case mbr of
            Just [r] -> return $ elem (fromIntegral r) [m,d,u]
            _        -> return False

    -- manage all my scratchpads based on the rules listed in the datatypes
    manageScratchPads :: ManageHook
    manageScratchPads = namedScratchpadManageHook myScratchPads


------------------------------------------------------------------------
-- ScratchPads
--
myScratchPads = [ NS "mixer"    spawnMixer findMixer manageMixer
                , NS "terminal" spawnTerm  findTerm  manageTerm
                ]

  where
    spawnMixer  = "ossxmix"
    findMixer   = className =? "Ossxmix"
    manageMixer = customFloating $ W.RationalRect l t w h

      where
        h = 0.6       -- height, 60%
        w = 0.6       -- width, 60%
        t = (1 - h)/2 -- centered top/bottom
        l = (1 - w)/2 -- centered left/right

    spawnTerm  = myTerminal ++ " -name scratchpad"
    findTerm   = resource  =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h

      where
        h = 0.5       -- height, 10%
        w = 0.5       -- width, 100%
        t = (1 - h)/2 -- centered top/bottom
        l = (1 - w)/2 -- centered left/right


------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.

-- Pretty Printing
-- . wrap ("^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/eye_l.xbm)") ""
-- . wrap ("^i(" ++ myIconDir ++ "/has_win.xbm)") ""

-- myDzenPP h = defaultPP -- the h here...
myLogHook h ico = dynamicLogWithPP $ defaultPP     -- the h here...
  { -- current workspace
    ppCurrent         = dzenColor colorFG5 colorFG . pad .
                        wrap ("^i(" ++ ico ++ "/has_win.xbm)") ""
    -- other workspaces with windows
  , ppHidden          = dzenColor colorFG5 colorBG . pad .
                        wrap ("^i(" ++ ico ++ "/has_win.xbm)") "" . noScratchPad
    -- other workspaces no windows
  , ppHiddenNoWindows = dzenColor colorFG2 colorBG . pad
    -- current layout
  , ppLayout          = dzenColor colorFG5 colorBG . --pad
                        (\x -> case x of
                            "Mirror Tiled" -> "^i(" ++ ico ++ "/mtall.xbm)"
                            "Spacing" -> "^i(" ++ ico ++ "/tall.xbm)"
                            "Tiled" -> "^i(" ++ ico ++ "/tall.xbm)"
                            "Full" -> "^i(" ++ ico ++ "/full.xbm)"
                            "Grid" -> "^i(" ++ ico ++ "/grid.xbm)"
                            "SimplestFloat" -> "~"
                            _ -> x
                        )
    -- window that needs attention
  , ppUrgent          = dzenColor colorFG4 colorBG . pad .
                        wrap ("^i(" ++ ico ++ "/alert.xbm)") "" . dzenStrip
    -- current window's title
  , ppTitle           = dzenColor colorFG4 colorBG . shorten 50
  , ppWsSep           = ""                                -- separator between workspaces
  , ppSep             = " ^fg(" ++ colorFG4 ++ ")|^fg() " -- between each object
  , ppOutput          = hPutStrLn h                       -- ... must match the h here
  }
  where

    -- L needed for loggers
    dzenFG  c = dzenColor  c ""
    dzenFGL c = dzenColorL c ""

    -- filter out scratchpad function
    noScratchPad ws = if ws == "NSP" then "" else ws


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- startup :: X ()
-- startup = do
--           spawn "source $HOME/.xmonad/autostart.sh"
myStartupHook = ewmhDesktopsStartup >> setWMName "LG3D"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launchers
    , ((modm,               xK_w     ), spawn myDmenuBar)
    , ((modm,               xK_p     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_p     ), spawn "kupfer")

    -- launch apps
    , ((modm,               xK_a     ), spawn "nautilus ~/")
    -- , ((modm .|. shiftMask, xK_a     ), spawn "nautilus --browser ~/")
    , ((modm .|. shiftMask, xK_f     ), spawn "firefox")
    , ((modm .|. shiftMask, xK_t     ), spawn "thunderbird")
    , ((modm,               xK_Print ), spawn "scrot -q 90 ~/Images/Screenshots/%F-%T.png")

    , ((modm,               xK_z     ), withFocused (sendMessage . maximizeRestore))

    -- close focused window
    , ((modm,               xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- scratchpad
    , ((modm,               xK_s     ), scratchTerm)

    -- Move focus to the next/previous/master window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master/next/previous window
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink/Expand the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- Increment/Deincrement the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_semicolon), sendMessage (IncMasterN (-1)))

    -- cycle workspaces
    , ((modm,               xK_Right ),  nextWS)
    , ((modm,               xK_Left  ),  prevWS)
    , ((modm .|. shiftMask, xK_Left  ),  shiftToPrev)
    , ((modm .|. shiftMask, xK_Right ),  shiftToNext)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit or restart xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
--     , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm              , xK_q     ), spawn myRestart)
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    where

      myRestart     = "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                      "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                      "xmonad --recompile && xmonad --restart"

      scratchTerm   = namedScratchpadAction myScratchPads "terminal"



-- vim:ts=2:sw=2:ai:et
