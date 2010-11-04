--
-- ~/.xmonad/xmonad.hs by pbrisbin
--
-- 14 july 2010 (fun with algebraic data types)
--
-- darcs broke on me so i've gone repo for now...
--
-- :: Versions I'm using:
--
--    ghc                       6.12.1-4
--    haskell-mtl               1.1.0.2-3
--    haskell-utf8-string       0.3.6-3
--    haskell-x11               1.5.0.0-2
--    haskell-x11-xft           0.3-14
--    xmonad                    0.9.1-4
--    xmonad-contrib            0.9.1-3
--    dzen2-svn                 271-1
--

-- Imports {{{
import XMonad hiding ((|||))

import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.UpdatePointer

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators (JumpToLayout)
import XMonad.Layout.LayoutHints (layoutHintsWithPlacement)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers (maildirNew,logCmd,dzenColorL,wrapL,shortenL)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ratio

import System.IO

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

-- }}}

-- Main {{{
main = do
  spawn "conky -c ~/.conkyrc-pbrisbin"
  spawn . show $ myRightBar
  d <- spawnPipe . show $ myLeftBar

  -- ewmh just makes wmctrl work
  xmonad $ ewmh $ withUrgencyHookC myUrgencyHook myUrgencyConfig $ defaultConfig
    { terminal           = myTerminal
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , modMask            = myModMask
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , logHook            = myLogHook d
    } `additionalKeysP` myKeys

-- }}}

-- Options {{{
--
-- if you change workspace names, be sure to update them throughout
--
myTerminal           = "urxvtc"
myWorkspaces         = ["1-main","2-web","3-chat"] ++ map show [4..9]
myBorderWidth        = 3
myModMask            = mod4Mask

myNormalBorderColor  = colorFG
myFocusedBorderColor = colorFG4

-- aur/dzen2-svn required for xft fonts
--myFont               = "Verdana-8"
myFont               = "-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*"

-- background/foreground and various levels of emphasis
colorBG              = "#303030"
colorFG              = "#606060"
colorFG2             = "#909090"
colorFG3             = "#c4df90"
colorFG4             = "#cc896d" -- limey
colorFG5             = "#c4df90" -- peachy
colorFG6             = "#ffffba" -- yellowy

barHeight            = 17
leftBarWidth         = 800
rightBarWidth        = 640

-- }}}

-- Layouts {{{
--
-- see http://pbrisbin.com:8080/pages/im-layout.html#update
--
myLayout = avoidStruts $ onWorkspace "3-chat" imLayout $ standardLayouts

  where

    standardLayouts = tiled ||| Mirror tiled ||| full

    full  = hinted $ noBorders Full
    tiled = hinted $ ResizableTall nmaster delta ratio []

      where

        nmaster = 1
        delta   = 3/100
        ratio   = toRational $ 2/(1 + sqrt 5 :: Double) -- golden ratio

    hinted l = layoutHintsWithPlacement (0,0) l

    -- im roster on left tenth, standardLayouts in other nine tenths
    imLayout = withIM (1/10) imProp standardLayouts
    imProp   = Role "roster"


-- }}}

-- ManageHook {{{
myManageHook = (composeAll . concat $
  [ [resource  =? r                 --> doIgnore         |  r    <- myIgnores] -- ignore desktop
  , [className =? c                 --> doShift "2-web"  |  c    <- myWebs   ] -- move webs to web
  , [title     =? t                 --> doShift "3-chat" |  t    <- myChats  ] -- move chats to chat
  , [className =? c                 --> doShift "3-chat" | (c,_) <- myIMs    ] -- move chats to chat
  , [className =? c <&&> role /=? r --> doFloat          | (c,r) <- myIMs    ] -- float all ims but roster
  , [className =? c                 --> doFloat          |  c    <- myFloats ] -- float my floats
  , [className =? c                 --> doCenterFloat    |  c    <- myCFloats] -- float my floats
  , [name      =? n                 --> doFloat          |  n    <- myNames  ] -- float my names
  , [name      =? n                 --> doCenterFloat    |  n    <- myCNames ] -- float my names
  , [isFullscreen                   --> myDoFullFloat                        ]
  ]) <+> manageTypes <+> manageDocks <+> manageScratchPads

  where

    role = stringProperty "WM_WINDOW_ROLE"
    name = stringProperty "WM_NAME"

    -- [ ("class1","role1"), ("class2","role2"), ... ]
    myIMs     = [("Gajim.py","roster")]
    myChats   = ["irssi","mutt"]
    myFloats  = ["MPlayer","Zenity","VirtualBox","rdesktop"]
    myCFloats = ["Xmessage","Save As...","XFontSel"]
    myWebs    = ["Navigator","Shiretoko","Firefox"]     ++ -- firefox
                ["Uzbl","uzbl","Uzbl-core","uzbl-core"] ++ -- uzbl
                ["Jumanji","jumanji"]                   ++ -- jumanji
                ["Google-chrome","Chromium"]               -- chrom(e|ium)
    myIgnores = ["desktop","desktop_window"]
    myNames   = ["Google Chrome Options","Chromium Options"]
    myCNames  = ["bashrun"]

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

-- }}}

-- ScratchPads {{{
--
-- see http://pbrisbin.com:8080/pages/xmonad-scratchpad2.html
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

        h = 0.1       -- height, 10%
        w = 1         -- width, 100%
        t = 1 - h     -- bottom edge
        l = (1 - w)/2 -- centered left/right


-- }}}

-- Status Bars {{{
--
-- see http://pbrisbin.com:8080/pages/xmonad_status.html#update_2
--
data TextAlign = LeftAlign
               | RightAlign
               | Centered

instance Show TextAlign where
  show LeftAlign  = "l"
  show RightAlign = "r"
  show Centered   = "c"

data Dzen = Dzen { x_offset  :: Int
                 , y_offset  :: Int
                 , width     :: Int
                 , height    :: Int
                 , alignment :: TextAlign
                 , font      :: String
                 , fg_color  :: String
                 , bg_color  :: String
                 , input     :: String
                 }

instance Show Dzen where
  show dzen = let Dzen { x_offset  = x
                       , y_offset  = y
                       , width     = w
                       , height    = h
                       , alignment = a
                       , font      = f
                       , fg_color  = fg
                       , bg_color  = bg
                       , input     = i
                       } = dzen

              in trim . unwords $ [ i
                                  , "dzen2"
                                  , "-p"
                                  , "-fn", quote f
                                  , "-fg", quote fg
                                  , "-bg", quote bg
                                  , "-ta", show  a
                                  , "-x" , show  x
                                  , "-y" , show  y
                                  , "-w" , show  w
                                  , "-h" , show  h
                                  , "-e 'onstart=lower'"
                                  ]

    where

      quote :: String -> String
      quote s = "'" ++ s ++ "'"

myLeftBar, myRightBar :: Dzen
myLeftBar  = Dzen 0            0 leftBarWidth  barHeight LeftAlign  myFont colorFG colorBG []
myRightBar = Dzen leftBarWidth 0 rightBarWidth barHeight RightAlign myFont colorFG colorBG "conky -c ~/.xmonad/dzen_conkyrc |"

-- }}}

-- LogHook {{{
myLogHook :: Handle -> X ()
myLogHook h = (dynamicLogWithPP $ defaultPP
  { ppCurrent         = dzenFG colorFG5 . pad
  , ppVisible         = dzenFG colorFG2 . pad
  , ppUrgent          = dzenFG colorFG4 . pad . dzenStrip
  , ppLayout          = dzenFG colorFG2 . myRename
  , ppHidden          = dzenFG colorFG2 . noScratchPad
  , ppHiddenNoWindows = namedOnly
  , ppTitle           = shorten 100
  , ppSort            = getSortByXineramaRule
  , ppExtras          = [myMail] -- , myUpdates, myTorrents]
  , ppSep             = replicate 4 ' '
  , ppWsSep           = []
  , ppOutput          = hPutStrLn h
  }) >> updatePointer (Relative 0.95 0.95)

  where

    -- thanks byorgey (this filters out NSP too)
    namedOnly ws = if any (`elem` ws) ['a'..'z'] then pad ws else ""

    -- my own filter out scratchpad function
    noScratchPad ws = if ws == "NSP" then "" else pad ws

    -- L needed for loggers
    dzenFG  c = dzenColor  c ""
    dzenFGL c = dzenColorL c ""

    -- custom loggers
    myMail     = wrapL "Mail: " ""  . dzenFGL colorFG6 $ maildirNew "/home/me/mail/INBOX"
    myUpdates  = logCmd "$HOME/.bin/logger-updates"
    myTorrents = logCmd "$HOME/.bin/logger-torrents"

    myRename = (\x -> case x of
               "Hinted ResizableTall"          -> "/ /-/"
               "Mirror Hinted ResizableTall"   -> "/-,-/"
               "Hinted Tabbed Bottom Simplest" -> "/.../"
               "Hinted TwoPane"                -> "/ / /"
               "Hinted Full"                   -> "/   /"
               _                               -> x
               ) . stripIM

    stripIM s = if "IM " `isPrefixOf` s then drop (length "IM ") s else s

-- }}}

-- My SpawnHook {{{
--
-- spawn an arbitrary command on urgent
--
data MySpawnHook = MySpawnHook String deriving (Read, Show)

instance UrgencyHook MySpawnHook where
  urgencyHook (MySpawnHook s) w = spawn s

-- 'ding!'
myUrgencyHook :: MySpawnHook
myUrgencyHook = MySpawnHook "ossplay -q /usr/share/gajim/data/sounds/message2.wav"

-- show hooks even on nonfocused but visible screen, and remind me every
-- 30 seconds if i don't listen -- for some reason, ppUrgent doesn't
-- work if it's a visible/nonfocused screen even though i'm using
-- dzenStrip
myUrgencyConfig :: UrgencyConfig
myUrgencyConfig = UrgencyConfig OnScreen (Repeatedly 1 30)

-- }}}

-- Key Bindings {{{
--
-- only those which override/change defaults
--
-- see http://pbrisbin.com:8080/pages/scripts.html
--
myKeys = [ ("M-p"                   , spawn "launcher"        ) -- dmenu app launcher
         , ("M-S-p"                 , spawn "bashrun"         ) -- gmrun replacement

         -- opening apps with Win
         , ("M4-t"                  , scratchTerm             ) -- bring me a term
         , ("M4-S-m"                , scratchMixer            ) -- bring me a mixer
         , ("M4-m"                  , spawn myMail            ) -- open mail client
         , ("M4-b"                  , spawn myBrowser         ) -- open web client
         , ("M4-i"                  , spawn myIRC             ) -- open/attach IRC client in screen
         , ("M4-r"                  , spawn myTorrents        ) -- open/attach rtorrent in screen
         , ("M4-e"                  , spawn myEject           ) -- open/close tray
         , ("M4-l"                  , spawn myLock            ) -- W-l to lock screen

         -- some custom hotkeys
         , ("M-a"                   , spawn "msearch all"     ) -- search current playlist via dmenu
         , ("M-g"                   , spawn "goodsong"        ) -- note current song as 'good'
         , ("M-S-g"                 , spawn "goodsong -p"     ) -- play a random 'good' song
         , ("<Print>"               , spawn "sshot"           ) -- take a screenshot

         -- extended workspace navigations
         , ("M-`"                   , toggleWS                ) -- switch to the most recently viewed ws
         , ("M-<Backspace>"         , focusUrgent             ) -- focus most recently urgent window
         , ("M-S-<Backspace>"       , clearUrgents            ) -- make urgents go away
         , ("M-0"                   , viewEmptyWorkspace      ) -- go to next empty workspace
         , ("M-S-0"                 , tagToEmptyWorkspace     ) -- targ window to empty workspace and view it

         -- extended window movements
         , ("M-o"                   , sendMessage MirrorShrink) -- shink slave panes vertically
         , ("M-i"                   , sendMessage MirrorExpand) -- expand slave panes vertically
         , ("M-f"                   , jumpToFull              ) -- jump to full layout

         -- non-standard navigation
         , ("M-h"                   , focusScreen 0           ) -- focus left screen
         , ("M-l"                   , focusScreen 1           ) -- focus rght screen
         , ("M-S-h"                 , sendMessage Shrink      ) -- shrink master (was M-h)
         , ("M-S-l"                 , sendMessage Expand      ) -- expand master (was M-l)

         -- mpd and oss volume
         , ("<XF86AudioPlay>"       , spawn "mpc toggle"      ) -- play/pause mpd
         , ("<XF86AudioStop>"       , spawn "mpc stop"        ) -- stop mpd
         , ("<XF86AudioPrev>"       , spawn "mpc prev"        ) -- prev song
         , ("<XF86AudioNext>"       , spawn "mpc next"        ) -- next song
         , ("<XF86AudioMute>"       , spawn "ossvol -t"       ) -- toggle mute
         , ("<XF86AudioLowerVolume>", spawn "ossvol -d 1"     ) -- volume down
         , ("<XF86AudioRaiseVolume>", spawn "ossvol -i 1"     ) -- volume up

         -- Mod+ to control MPlayer
         , ("M-<XF86AudioPlay>"     , mPlay "pause"           ) -- play/pause mplayer
         , ("M-<XF86AudioStop>"     , mPlay "stop"            ) -- stop mplayer
         , ("M-<XF86AudioPrev>"     , mPlay "seek -10"        ) -- seek back 10s
         , ("M-<XF86AudioNext>"     , mPlay "seek 10"         ) -- seek forward 10s

         -- kill, reconfigure, exit commands
         , ("M4-q"                  , killAll                 ) -- close all windows on this ws
         , ("M-q"                   , spawn myRestart         ) -- restart xmonad
         , ("M-S-q"                 , spawn "leave"           ) -- logout/shutdow/restart menu
         ]

         where

           focusScreen n = screenWorkspace n >>= flip whenJust (windows . W.view)

           jumpToFull    = sendMessage $ JumpToLayout "Hinted Full"

           scratchTerm   = namedScratchpadAction myScratchPads "terminal"
           scratchMixer  = namedScratchpadAction myScratchPads "mixer"

           myBrowser     = "$BROWSER"
           myLock        = "slock"
           myEject       = "eject -T"
           myMail        = myTerminal ++ " -e mutt"
           myIRC         = myScreen "irssi"
           myTorrents    = myScreen "rtorrent"

           -- see http://pbrisbin.com:8080/pages/screen_tricks.html
           myScreen s    = myTerminal ++ " -title "                    ++ s
                                      ++ " -e bash -cl \"SCREEN_CONF=" ++ s
                                      ++ " screen -S "                 ++ s
                                      ++ " -R -D "                     ++ s
                                      ++ "\""

           -- see http://pbrisbin.com:8080/pages/mplayer-control.html
           mPlay s       = spawn $ unwords [ "echo", s, "> $HOME/.mplayer_fifo" ]

           -- kill all conky/dzen2 before executing default restart command
           myRestart     = "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                           "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                           "xmonad --recompile && xmonad --restart"

-- }}}

-- vim:foldmethod=marker foldmarker={{{,}}}
