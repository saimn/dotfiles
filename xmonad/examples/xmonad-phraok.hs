-- xmonad&contrib darcs version 22-12-2007
-- import the necessary libraries

import XMonad
import XMonad.ManageHook
import XMonad.Operations (kill, windows, sendMessage)
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
--import XMonad.Actions.RotSlaves
--import XMonad.Actions.RotView
import XMonad.Actions.SinkAll
import XMonad.Hooks.DynamicLog   ( PP(..), dynamicLogWithPP, dzenColor, wrap, defaultPP )
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders   ( noBorders, smartBorders )
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.PerWorkspace
--import XMonad.Hooks.SetWMName
--import XMonad.Hooks.UrgencyHook--(withUrgencyHook, NoUrgencyHook(..))
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Ratio
import Graphics.X11
--import Graphics.X11.Xlib.Event
--import Graphics.X11.Xlib
--import Graphics.X11.Xlib.Extras
import System.IO
--statusBarCmd= "/home/simon/.dzen/menu.sh"
statusBarCmd= "dzen2 -bg '#222222' -fg '#FFFFFF' -h 14 -w 580 -sa c -e '' -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -ta l"

main = do din <- spawnPipe statusBarCmd
          xmonad $ defaultConfig

                     { borderWidth        = 1
                     , normalBorderColor  = "#444444"
                     , focusedBorderColor = "#444444"
                     , workspaces         = myWorkspaces
                     , terminal           = "urxvtc"
                     , modMask            = mod1Mask
                     , defaultGaps        = [(14,0,0,0)]
                     , manageHook            = manageHook defaultConfig <+> myManageHook
                     , logHook            = dynamicLogWithPP $ myPP din
                     , layoutHook         =  windowNavigation (tiled ||| Mirror tiled ||| noBorders (Full) ||| Grid)
                     , keys               = \c -> myKeys c `M.union` keys defaultConfig c
                     , mouseBindings      = \c -> myMouse c `M.union` mouseBindings defaultConfig c
                     }
                     where
                     tiled   = Tall nmaster delta ratio

                     -- The default number of windows in the master pane
                     nmaster = 1

                     -- Default proportion of screen occupied by master pane
                     ratio   = 2/(1+(toRational(sqrt(5)::Double))) -- golden

                     -- Percent of screen to increment by when resizing panes
                     delta   = 2%100
-- application control

myWorkspaces :: [String]
myWorkspaces = ["1:main", "2:web", "3:im", "4:other"] -- ++ map show [7 .. 9 :: Int]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className   =? c                 --> doFloat | c <- myFloats]
    , [ title       =? t                 --> doFloat | t <- myOtherFloats]
    , [ resource    =? r                 --> doIgnore | r <- myIgnores]
    , [ className   =? "Firefox-bin"	--> doF (W.shift "2:web") ]
    , [ className   =? "Opera"	--> doF (W.shift "2:web") ]
    , [ className   =? "Pan"	--> doF (W.shift "4:other") ]
    , [ className   =? "Pidgin"	--> doF (W.shift "3:im") ]
    , [ className   =? "Gimp"	--> doF (W.shift "4:other") ]
    ]
    where
        myIgnores       = []--"panel", "stalonetray", "trayer"
        myFloats        = ["feh", "GIMP", "gimp", "gimp-2.4", "Galculator",  "Mplayer", "xine", "MediaPlayer", "VLC media player", "Gpicview", "Pidgin", "Buddy List"]
        myOtherFloats   = ["Bon Echo Preferences", "Bookmarks", "Downloads", "Add-ons", "Bon Echo - Restore Previous Session", "Open...", "Bibliothèque Multimédia", "VLC media player", "Delete"]

-- modify/add default key binds
--
myKeys (XConfig {modMask = modm}) = M.fromList $
           [
           -- custom dmenu
           ((modm, xK_d), spawn "exe=`dmenu_path | dmenu -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#222222' -nf '#FFFFFF' -sb '#0066ff'` && eval \"exec $exe\"") -- %! Launch dmenu
           -- sink all floating windows
           ,  ((modm .|. shiftMask, xK_t), sinkAll)
           -- swap focused with master, or master with next in line
           , ((modm, xK_Return), dwmpromote)
           , ((modm, xK_KP_Enter), dwmpromote)
           -- rotate slave clients
           , ((modm .|. shiftMask, xK_Tab   ), rotSlavesUp)
           -- cycle through non-empty workspaces
           --, ((modm .|. shiftMask, xK_Right), rotView True)
           --, ((modm .|. shiftMask, xK_Left), rotView False)
	   -- cycle workspaces
	   , ((shiftMask, xK_Right), nextWS)
	   , ((shiftMask, xK_Left),  prevWS)
           -- switch to previous workspace
           , ((modm, xK_z), toggleWS)
           -- toggle to fullscreen.
           , ((modm, xK_x), sendMessage ToggleLayout)
           -- session management
	   , ((modm .|. shiftMask,               xK_w     ), kill  )
	   , ((modm .|. controlMask .|. shiftMask, xK_End), spawn "sudo shutdown -h now")
	   , ((modm .|. controlMask .|. shiftMask, xK_Delete), spawn "sudo shutdown -r now")
	   -- Move windows in workspace
	   , ((modm, xK_Right), sendMessage $ Go R)
	   , ((modm, xK_Left ), sendMessage $ Go L)
	   , ((modm, xK_Up   ), sendMessage $ Go U)
	   , ((modm, xK_Down ), sendMessage $ Go D)
	   , ((modm .|. controlMask, xK_Right), sendMessage $ Swap R)
	   , ((modm .|. controlMask, xK_Left ), sendMessage $ Swap L)
	   , ((modm .|. controlMask, xK_Up   ), sendMessage $ Swap U)
	   , ((modm .|. controlMask, xK_Down ), sendMessage $ Swap D)
           -- application hotkeys
	   , ((modm,	 xK_Print), spawn "exec scrot desktop-%d-%m_%H:%M:%S.png -q 85 -e \'mv $f /home/simon/Desktop/\'")
	   , ((modm, xK_f), spawn "exec firefox")
          -- , ((modm, xK_m), spawn "exec medit")
           , ((modm, xK_m), spawn "exec urxvtc -e mc")
	   , ((modm, xK_t), spawn "exec thunar")
	   , ((modm, xK_r), spawn "exec rox")
	   , ((modm, xK_p), spawn "exec pidgin")
	   , ((modm, xK_g), spawn "exec gimp")
	   , ((modm, xK_v), spawn "exec vlc")
	   , ((modm, xK_x), spawn "exec xine")
	   , ((modm, xK_KP_Prior), spawn "exec xmms")
	   , ((modm .|. shiftMask, xK_m), spawn "exec killall mpd")
	    ]
	         ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((modm .|. m, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_F1 .. xK_F6]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- modify/add default mouse binds
--
myMouse (XConfig {modMask = modm}) = M.fromList $
           [ ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
           --, ((modm, button5), (\_ -> rotView True))
           --, ((modm, button4), (\_ -> rotView False))
	   , ((shiftMask, button5), (\_ -> nextWS))
           , ((shiftMask, button4), (\_ -> prevWS))
           --, ((0, button7), (\_ -> nextWS))
           --, ((0, button6), (\_ -> prevWS))
	]
-- dynamiclog pretty printer for dzen
--
myPP h = defaultPP
                 { ppCurrent = wrap "^fg(#99CCFF)^bg(#444444)^p(2)^i(/home/simon/.dzen/bitmaps/has_win.xbm)^fg()" "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId    -- Trim the '[Int]:' from workspace tags
                  , ppVisible = wrap "^bg(grey30)^fg(grey75)^p(2)" "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId    -- Trim the '[Int]:' from workspace tags
                  , ppHidden = wrap "^fg(#ff)^bg()^p(2)^i(/home/simon/.dzen/bitmaps/has_win.xbm)^fg()" "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId    -- Trim the '[Int]:' from workspace tags
                  , ppHiddenNoWindows = wrap "^bg()^fg(#919191)^p(2)^i(/home/simon/.dzen/bitmaps/has_no_app.xbm)" "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId    -- Trim the '[Int]:' from workspace tags
                  , ppSep     = "  ^fg(#99CCFF)^r(2x2)^fg()  "
                  , ppWsSep           = "^i(/home/simon/.dzen/bitmaps/has_no_app.xbm)"
                  , ppLayout  = dzenColor "#FFFFFF" "" .
                                (\x -> case x of
                                         "Tall" -> " tall ^fg(#99CCFF)^i(/home/simon/.dzen/bitmaps/tall.xbm)^fg()"
                                         "Mirror Tall" -> "mirror ^fg(#99CCFF)^i(/home/simon/.dzen/bitmaps/mtall.xbm)^fg()"
                                         "Full" -> " full ^fg(#99CCFF)^i(/home/simon/.dzen/bitmaps/full.xbm)^fg()"
                                         "Grid" -> " grid ^fg(#99CCFF)^i(/home/simon/.dzen/bitmaps/grid.xbm)^fg()"
                                         "Tabbed" -> "tabbed ^fg(#99CCFF)^i(/home/simon/.dzen/bitmaps/tab.xbm)^fg()"
                                )
--                  , ppTitle   = dzenColor "white" "" . wrap "< " " >"
                  , ppTitle   = dzenColor "white" ""
                  , ppOutput   = hPutStrLn h
                  }
