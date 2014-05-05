-- Ondřej Súkup xmonad config ...

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.Dmenu
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhDesktopsEventHook)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Util.Themes
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["con","web","irc","sublime","steam"] ++ map show [6 .. 9]

------------------------------------------------------------------------
-- key bindings from XMonad.Config.Arossato...

defKeys    = keys defaultConfig

delKeys x  = foldr M.delete           (defKeys x) (toRemove x)

newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
-- remove some of the default key bindings
-- Ximerama handling and gmrun
toRemove x =
	[ (modMask x              , xK_w)
	, (modMask x              , xK_e)
	, (modMask x              , xK_r)
	, (modMask x .|. shiftMask, xK_w)
	, (modMask x .|. shiftMask, xK_e)
	, (modMask x .|. shiftMask, xK_r)
	, (modMask x .|. shiftMask, xK_p)
	]
-- These are my personal key bindings
-- Grid select + screnshot
toAdd x   =
	[ ((modMask x              , xK_g  	  ), goToSelected      defaultGSConfig   )
	, ((modMask x              , xK_Print ), spawn "scrot '%F-%H-%M-%S.png' -e 'mv $f ~/Shot/'")
  	, ((modMask x              , xK_s     ), scratchpadSpawnAction defaultConfig)
  	, ((modMask x .|. shiftMask, xK_p	), submap . M.fromList $
	  	[(( 0, xK_q ),	spawn "quasselclient"		)
		,(( 0, xK_c ),	spawn "google-chrome-stable"	)
		,(( 0, xK_s ), 	spawn "sublime_text"		)
		,(( 0, xK_g ),  spawn "steam"			)
		])
	]
------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ smartBorders $ tabbed shrinkText ( theme smallClean) ||| Full ||| tiled
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = (2/(1+(toRational(sqrt(5)::Double))))

     -- Percent of screen to increment by when resizing panes
     delta   = 5/100

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
myManageHook :: ManageHook
myManageHook = composeAll $
				 [isDialog --> doFloat]
				 ++
                 [ resource  =? r --> doIgnore | r <- myIgnores ]
                 ++
                   -- auto-float certain windows
                 [ className =? c --> doCenterFloat | c <- myFloats ]
                 ++
                   -- send certain windows to certain workspaces
                 [ className =? c --> doF (W.shift "web") | c <- myWebS ]
                 ++
                 [ className =? c --> doF (W.shift "con") | c <- myConsole ]
                 ++
                 [ className =? c --> doF (W.shift "irc") | c <- myIRC ]
                 ++
                 [ className =? c --> doF (W.shift "steam") | c <- mySteam]
                 ++
                 [ className =? c --> doF (W.shift "sublime") | c <- myText ]
                 ++
                 [ isFullscreen   --> doFullFloat ]
                 ++
                   -- unmanage docks such as gnome-panel and dzen
                 [ manageDocks
                 , fullscreenManageHook
                 , scratchpadManageHookDefault
                 ]
    -- windows to operate
    where myIgnores = [ "desktop","kdesktop", "desktop_window" ]
          myFloats  = [ "Steam"
                      , "steam"
                      , "vlc"
                      , "Vlc"
                      , "mpv"
                      ]
          myWebS    = ["Google-chrome-stable"]
          myConsole = ["URxvt"]
          myIRC     = ["Quasselclient"]
          mySteam   = ["Steam","steam"]
          myText    = ["Sublime_text"]

------------------------------------------------------------------------
-- Event handling
--
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
--- myEventHook = mempty
myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
--
main = do 
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh defaults {
    	logHook = dynamicLogWithPP $ xmobarPP{ ppOutput = hPutStrLn xmproc }
    	}



defaults = defaultConfig {
      -- simple stuff
        terminal           = "urxvt",
        --focusFollowsMouse  = myFocusFollowsMouse,
        --clickJustFocuses   = myClickJustFocuses,
        borderWidth        = 2,
        modMask            = mod4Mask,
        workspaces         = myWorkspaces,
        --normalBorderColor  = myNormalBorderColor,
        --focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = newKeys,
        --mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,      
        manageHook         = myManageHook,
        handleEventHook    = myEventHook
        --startupHook        = myStartupHook
        }
