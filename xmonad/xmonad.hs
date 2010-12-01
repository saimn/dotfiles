
-- import XMonad
import XMonad hiding ( (|||) )

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ratio

import System.Environment (getEnv)
import System.Exit
import System.IO
import System.Posix.Unistd

import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleResize as Flex
-- import qualified Data.Map        as M

import Graphics.X11.Xlib
import IO (Handle, hPutStrLn)

-- utils
import XMonad.Util.EZConfig (additionalKeysP,additionalMouseBindings)
import XMonad.Util.Loggers (maildirNew,logCmd,dzenColorL,wrapL,shortenL)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.WithAll (killAll,sinkAll)

-- layouts
-- import XMonad.Layout.BorderResize
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators (JumpToLayout)
import XMonad.Layout.LayoutCombinators
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
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Actions.MouseResize
-- import XMonad.Layout.WindowArranger

------------------------------------------------------------------------
-- Main
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
   spawn "conky -c ~/.conkycolors/conkyrc"

   -- ewmh just makes wmctrl work
   xmonad $ ewmh $ withUrgencyHookC myUrgencyHook myUrgencyConfig $ defaultConfig
       { terminal           = myTerminal
       , borderWidth        = myBorderWidth
       , workspaces         = myWorkspaces
       , normalBorderColor  = myNormalBorderColor
       , focusedBorderColor = myFocusedBorderColor
       , modMask            = myModMask
       -- , keys               = myKeys
       , manageHook         = myManageHook
       , layoutHook         = myLayout
       , logHook            = myLogHook d (homedir ++ "/.dzen/bitmaps")
       -- , handleEventHook    = mappend myEventHook
       , startupHook        = myStartupHook
       } `additionalKeysP` myKeys `additionalMouseBindings` myMouse

------------------------------------------------------------------------
-- Simple stuff

myTerminal          = "urxvtc"
myWorkspaces        = ["main","mail","web","code"] ++ map show [5..9]
myBorderWidth       = 2

-- mod1Mask ("left alt"). mod3Mask ("right alt"). mod4Mask ("windows").
myModMask           = mod4Mask

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = colorFG
myFocusedBorderColor = colorFG4

-- aur/dzen2-svn required for xft fonts
barFont  = "-*-terminus-*-r-normal-*-*-140-*-*-*-*-iso8859-*"
-- barFont  = "terminus"
barXFont = "inconsolata:size=10:bold"
xftFont  = "xft:inconsolata-10"

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

-- for medium screen (1440px)
leftBarWidth         = "700"
rightBarWidth        = "600"

-- for wide screen (1920px)
leftBarWidthL        = "970"
rightBarWidthL       = "870"

-- Status bar (two bars displayed as one)
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

myPrompt :: XPConfig
myPrompt = defaultXPConfig { font              = xftFont
                           , bgColor           = colorBG
                           , fgColor           = colorFG2
                           , fgHLight          = colorFG4
                           , bgHLight          = colorFG
                           , promptBorderWidth = 0
                           , position          = Top
                           , height            = 18
                           , historySize       = 128 }

------------------------------------------------------------------------
-- Layouts:

-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
-- windowArrange $ borderResize $
myLayout = avoidStruts $ onWorkspace "9" bfloat $ layouts

  where

    layouts = smartBorders tiled ||| spaced ||| smartBorders (Mirror tiled) |||
              full ||| grid ||| bfloat

    spaced = named "Spacing" $ maximize $ hinted $ spacing 6 $ ResizableTall 1 (2/100) (1/2) []
    tiled  = named "Tiled"   $ maximize $ hinted $ ResizableTall 1 (2/100) (1/2) []
    grid   = named "Grid"    $ maximize $ hinted $ Grid
    full   = named "Full"    $ noBorders Full
    bfloat = named "Float"   $ mouseResize $ simplestFloat

    hinted l = layoutHintsWithPlacement (0.5,0.5) l

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
-- (placeHook simpleSmart) <+>
myManageHook = (composeAll . concat $
  [ [resource  =? r  --> doIgnore         |  r <- myIgnores] -- ignore
  , [className =? c  --> doShift "web"    |  c <- myWebs   ] -- webs
  , [resource  =? r  --> doShift "mail"   |  r <- myMails  ] -- mails, chats
  -- , [title     =? t  --> doShift "mail"   |  t <- myMails  ] -- mails, chats
  , [className =? c  --> doFloat          |  c <- myFloats ] -- floats
  , [className =? c  --> doCenterFloat    |  c <- myCFloats] -- centered floats
  , [title     =? t  --> doFloat          |  t <- myOtherFloats]
  , [name      =? n  --> doFloat          |  n <- myNames  ] -- names
  , [name      =? n  --> doCenterFloat    |  n <- myCNames ] -- centered names
  , [isFullscreen    --> myDoFullFloat                     ]
  , [className =? "Gimp-2.6"  --> doShift "9"              ] -- Gimp
  , [(className =? "Firefox" <&&> resource =? "Dialog") --> doCenterFloat]
  ]) <+> manageTypes <+> manageDocks <+> manageScratchPads

  where

    role = stringProperty "WM_WINDOW_ROLE"
    name = stringProperty "WM_NAME"

    myFloats      = ["gimp","gimp-2.6"] ++      -- image viewers
                    ["Zenity","file_properties","Ediff","Sonata"] ++
                    ["Gnome-agenda"]
    myCFloats     = ["feh","Xmessage","Save As...","XFontSel","gmrun"] ++
                    ["MPlayer","Gnome-mplayer","Vlc","Totem"] ++ -- media players
                    ["Gcalctool","Idl","Toplevel","Wicd-client.py"]
    myOtherFloats = ["Bibliothèque Multimédia"] ++
                    ["Bookmarks","Downloads","Add-ons","IDL"] ++
                    ["Téléchargements","Préférences de Firefox"] ++
                    ["Open...","Enregistrer sous...","Ouvrir","Delete"]
    myMails       = ["Mail","mutt","Gajim.py"]
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
myScratchPads = [ NS "terminal" spawnTerm  findTerm  manageScratch
                -- , NS "mixer"    spawnMixer findMixer manageScratch
                -- , NS "htop"     spawnHtop  findHtop  manageScratch
                ]

  where

    spawnTerm   = myTerminal ++ " -name scratchpad"
    findTerm    = resource  =? "scratchpad"

    -- spawnHtop   = myTerminal ++ " -name htop -e htop"
    -- findHtop    = resource  =? "htop"

    -- spawnMixer  = myTerminal ++ " -name alsamixer -e alsamixer"
    -- findMixer   = resource  =? "alsamixer"

    manageScratch = customFloating $ W.RationalRect l t w h

      where
        h = 0.6       -- height, 60%
        w = 0.6       -- width, 60%
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
  , ppLayout          = dzenColor colorFG5 colorBG . myRename
    -- window that needs attention
  , ppUrgent          = dzenColor colorFG4 colorBG . pad .
                        wrap ("^i(" ++ ico ++ "/alert.xbm)") "" . dzenStrip
    -- current window's title
  , ppTitle           = dzenColor colorFG4 colorBG . shorten 50
  , ppWsSep           = ""                                -- separator between workspaces
  , ppSep             = " "                               -- between each object
  , ppOutput          = hPutStrLn h                       -- ... must match the h here
  }
  where

    -- L needed for loggers
    -- dzenFG  c = dzenColor  c ""
    -- dzenFGL c = dzenColorL c ""

    -- filter out scratchpad function
    noScratchPad ws = if ws == "NSP" then "" else ws

    myRename = (\x -> case x of
                   "Mirror Tiled" -> "^i(" ++ ico ++ "/mtall.xbm)"
                   "Spacing"      -> "^i(" ++ ico ++ "/tall.xbm)"
                   "Tiled"        -> "^i(" ++ ico ++ "/tall.xbm)"
                   "Full"         -> "^i(" ++ ico ++ "/full.xbm)"
                   "Grid"         -> "^i(" ++ ico ++ "/grid.xbm)"
                   "Float"        -> "~"
                   _ -> x
               )

------------------------------------------------------------------------
-- Startup hook
--
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- startup :: X ()
-- startup = do
--           spawn "source $HOME/.xmonad/autostart.sh"
myStartupHook = ewmhDesktopsStartup >> setWMName "LG3D"

------------------------------------------------------------------------
-- My SpawnHook
--
-- spawn an arbitrary command on urgent
--
data MySpawnHook = MySpawnHook String deriving (Read, Show)

instance UrgencyHook MySpawnHook where
  urgencyHook (MySpawnHook s) w = spawn s

-- 'ding!'
myUrgencyHook :: MySpawnHook
myUrgencyHook = MySpawnHook "aplay -q /usr/share/sounds/purple/receive.wav"

-- show hooks even on nonfocused but visible screen, and remind me every
-- 30 seconds if i don't listen -- for some reason, ppUrgent doesn't
-- work if it's a visible/nonfocused screen even though i'm using
-- dzenStrip
myUrgencyConfig :: UrgencyConfig
myUrgencyConfig = UrgencyConfig OnScreen (Repeatedly 1 30)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
--     -- launch a terminal
--     [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
--      -- Rotate through the available layout algorithms
--     , ((modm,               xK_space ), sendMessage NextLayout)
--     --  Reset the layouts on the current workspace to default
--     , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
--     -- Push window back into tiling
--     , ((modm,               xK_t     ), withFocused $ windows . W.sink)
--     -- Resize viewed windows to the correct size
--     , ((modm,               xK_n     ), refresh)

--     -- Move focus to the next/previous/master window
--     , ((modm,               xK_Tab   ), windows W.focusDown)
--     , ((modm,               xK_j     ), windows W.focusDown)
--     , ((modm,               xK_k     ), windows W.focusUp  )
--     , ((modm,               xK_m     ), windows W.focusMaster  )

--     -- Swap the focused window and the master/next/previous window
--     , ((modm,               xK_Return), windows W.swapMaster)
--     , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
--     , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

--     -- Increment/Deincrement the number of windows in the master area
--     , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
--     , ((modm              , xK_semicolon), sendMessage (IncMasterN (-1)))
--     ]

myKeys = [ ("M-p"                   , spawn "gmrun"           ) -- app launcher
         -- , ("M-S-p"                 , spawn "kupfer"          ) -- app launcher
         -- , ("M-w"                   , spawn myDmenuBar        ) -- app launcher
         , ("M-x"                   , shellPrompt myPrompt    )

         -- opening apps with Win
         , ("M-a"                   , spawn "nautilus ~/"     ) -- browse folders
         , ("M-S-t"                 , spawn "thunderbird"     ) -- open mail client
         , ("M-S-f"                 , spawn "firefox"         ) -- open web client
         -- , ("M-S-l"                 , spawn myLock            ) -- W-l to lock screen
         , ("M-<Print>"             , spawn myPrint           ) -- print screen
         , ("M-s"                   , scratchTerm             ) -- bring me a term
         -- , ("M-S-m"                 , scratchMixer            ) -- bring me a mixer
         -- , ("M-S-h"                 , scratchHtop             ) -- bring me a htop

         -- cycle workspaces
         , ("M-<Right>"             , nextWS                  )
         , ("M-<Left>"              , prevWS                  )
         , ("M-S-<Left>"            , shiftToPrev             )
         , ("M-S-<Right>"           , shiftToNext             )

         -- extended workspace navigations
         , ("M-`"                   , toggleWS                ) -- switch to the most recently viewed ws
         , ("M-<Backspace>"         , focusUrgent             ) -- focus most recently urgent window
         , ("M-S-<Backspace>"       , clearUrgents            ) -- make urgents go away
         , ("M-0"                   , viewEmptyWorkspace      ) -- go to next empty workspace
         , ("M-S-0"                 , tagToEmptyWorkspace     ) -- targ window to empty workspace and view it

         -- extended window movements
         , ("M-h"                   , sendMessage Shrink      ) -- shink slave panes vertically
         , ("M-l"                   , sendMessage Expand      ) -- expand slave panes vertically
         , ("M-S-h"                 , sendMessage MirrorShrink) -- shink slave panes vertically
         , ("M-S-l"                 , sendMessage MirrorExpand) -- expand slave panes vertically
         , ("M-f"                   , jumpToFull              ) -- jump to full layout
         , ("M-z"                   , maxWin                  ) -- maximize window
         , ("M-b"                   , sendMessage ToggleStruts) -- toggle the status bar gap
         , ("M-S-t"                 , sinkAll                 ) -- tile all windows
         , ("M-w"                   , placeFocused simpleSmart)

         -- mpd and oss volume
         -- , ("<XF86AudioPlay>"       , spawn "mpc toggle"      ) -- play/pause mpd
         -- , ("<XF86AudioStop>"       , spawn "mpc stop"        ) -- stop mpd
         -- , ("<XF86AudioPrev>"       , spawn "mpc prev"        ) -- prev song
         -- , ("<XF86AudioNext>"       , spawn "mpc next"        ) -- next song
         -- , ("<XF86AudioMute>"       , spawn "ossvol -t"       ) -- toggle mute
         -- , ("<XF86AudioLowerVolume>", spawn "ossvol -d 1"     ) -- volume down
         -- , ("<XF86AudioRaiseVolume>", spawn "ossvol -i 1"     ) -- volume up

         -- kill, reconfigure, exit commands
         , ("M-c"                   , kill                    ) -- close focused window
         , ("M-S-c"                 , killAll                 ) -- close all windows on this ws
         , ("M-q"                   , spawn myRestart         ) -- restart xmonad
         , ("M-S-q"                 , io (exitWith ExitSuccess)) -- logout/shutdow/restart menu
         -- , ("M-S-q"                 , spawn "leave"           ) -- logout/shutdow/restart menu
         ]
         ++
         -- mod-[F1..F9], Switch to workspace N
         -- mod-shift-[F1..F9], Move client to workspace N
         [ (otherModMasks ++ "M-" ++ key, action tag)
             | (tag, key)  <- zip myWorkspaces (map (\x -> "<F" ++ show x ++ ">") [1..12])
             , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
                                          , ("S-", windows . W.shift)]
         ]

         where

           jumpToFull    = sendMessage $ JumpToLayout "Full"
           maxWin        = withFocused (sendMessage . maximizeRestore)

           scratchTerm   = namedScratchpadAction myScratchPads "terminal"
           -- scratchMixer  = namedScratchpadAction myScratchPads "mixer"
           -- scratchHtop   = namedScratchpadAction myScratchPads "htop"

           myPrint       = "scrot -q 90 ~/Images/screenshots/%F-%T.png"
           -- myBrowser     = "$BROWSER"
           -- myLock        = "slock"
           -- myMail        = myTerminal ++ " -e mutt"
           -- myIRC         = myScreen "irssi"

           -- see http://pbrisbin.com:8080/pages/screen_tricks.html
           -- myScreen s    = myTerminal ++ " -title "                    ++ s
           --                            ++ " -e bash -cl \"SCREEN_CONF=" ++ s
           --                            ++ " screen -S "                 ++ s
           --                            ++ " -R -D "                     ++ s
           --                            ++ "\""

           -- see http://pbrisbin.com:8080/pages/mplayer-control.html
           -- mPlay s       = spawn $ unwords [ "echo", s, "> $HOME/.mplayer_fifo" ]

           -- kill all conky/dzen2 before executing default restart command
           myRestart     = "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                           "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                           "xmonad --recompile && xmonad --restart"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myMouse = [ ((mod4Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]



-- vim:ts=2:sw=2:ai:et
