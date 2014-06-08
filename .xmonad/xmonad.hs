-- Ondřej Súkup xmonad config ...

-- main import
import XMonad

-- system libs import
import System.Exit
import System.Environment
import System.FilePath.Posix
import System.FilePath.Find
import System.Directory


-- xmonad hooks
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
--xmonad actions
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
--xmonad utils
import XMonad.Util.Cursor
import XMonad.Util.CustomKeys
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce			(spawnOnce)
import XMonad.Util.Themes
--xmonad layouts
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen 

-- import xmonad promt
import XMonad.Prompt
import XMonad.Prompt.Shell


-- qualified imports of Data and Stack
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
--                Custom Xmonad.Promt from:
-- http://blog.tarn-vedra.de/2014/05/xmonad-loves-password-store.html
--    promt for Pass , CLI password storage manager 
------------------------------------------------------------------------

data Pass = Pass

instance XPrompt Pass where
	showXPrompt       Pass = "Pass: "
	commandToComplete _ c  = c
	nextCompletion      _  = getNextCompletion

passPrompt :: XPConfig -> X ()
passPrompt c = do
  	li <- io getPasswords
  	mkXPrompt Pass c (mkComplFunFromList li) selectPassword

selectPassword :: String -> X ()
selectPassword s = spawn $ "pass -c " ++ s

getPasswords :: IO [String]
getPasswords = do
  	home <- getEnv "HOME"
  	let passwordStore = home </> ".password-store"
  	entries <- find System.FilePath.Find.always (fileName ~~? "*.gpg") passwordStore
  	return $ map (makeRelative passwordStore . dropExtension) entries 

promptConfig = defaultXPConfig
  	{ font        = "xft:Source Code Pro:pixelsize=12"
  	, borderColor = "#1e2320"
  	, fgColor     = "#dddddd"
  	, fgHLight    = "#ffffff"
  	, bgColor     = "#1e2320"
  	, bgHLight    = "#5f5f5f"
  	, height      = 18
  	, position    = Top
  	}
------------------------------------------------------------------------
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["con","web","irc","sublime","steam"] ++ map show [6 .. 9]

------------------------------------------------------------------------
-- add mouse buttons
-- Mouse button  defs

button8 = 8 :: Button
button9 = 9 :: Button

defMouse     = mouseBindings defaultConfig

delMouse  x  = foldr M.delete            (defMouse x)  (toRemove'  x)

newMouse  x  = foldr (uncurry M.insert)  (delMouse x)  (toAdd'     x)

toRemove' x = []

toAdd' 	  x =
	[ ((0,  button8), const prevWS )
  , ((0,  button9), const nextWS )
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
myLayout = avoidStruts $ smartBorders $ fullscreenFloat Full ||| tabbed shrinkText ( theme smallClean ) ||| tiled
  	where
     	-- default tiling algorithm partitions the screen into two panes
    	tiled   = Tall nmaster delta ratio

     	-- The default number of windows in the master pane
     	nmaster = 1

     	-- Default proportion of screen occupied by master pane
     	ratio   = 2/(1+(toRational(sqrt 5::Double)))

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
-- myStartupHook
myStartupHook = do
	setDefaultCursor xC_left_ptr
	spawnOnce "google-chrome-stable"
	spawnOnce "urxvtc"
	spawnOnce "quasselclient"
	spawnOnce "steam"
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
--
main = do 
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh defaults {
    	logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmproc }
    	}



defaults = defaultConfig {
    -- simple stuff
    terminal           = "urxvtc",
    --focusFollowsMouse  = myFocusFollowsMouse,
    --clickJustFocuses   = myClickJustFocuses,
    borderWidth        = 2,
    modMask            = mod4Mask,
    workspaces         = myWorkspaces,
    --normalBorderColor  = myNormalBorderColor,
    --focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = customKeys delkeys inskeys,
    mouseBindings      = newMouse,

    -- hooks, layouts
    layoutHook         = myLayout,      
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    startupHook        = myStartupHook
    }
    where
      delkeys :: XConfig l -> [(KeyMask, KeySym)]
      delkeys XConfig {modMask = modm} =
      -- remove xinerama and default run keys 
           [ (modm .|. m, k) | (m, k) <- zip [0, shiftMask] [xK_w, xK_e, xK_r,xK_p] ]
      inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
      inskeys conf@(XConfig {modMask = modm}) =
      --add grid select, screenshot, scratchpad, run shortcuts
            [ ((modm                    , xK_g          ), goToSelected defaultGSConfig                       )
            , ((modm                    , xK_Print      ), spawn "scrot '%F-%H-%M-%S.png' -e 'mv $f ~/Shot/'" )
            , ((modm                    , xK_s          ), scratchpadSpawnAction conf                         )
            , ((modm  .|. controlMask   , xK_p          ), submap . M.fromList $
              [(( 0, xK_q ),  spawn "quasselclient"        )
              ,(( 0, xK_w ),  spawn "google-chrome-stable" )
              ,(( 0, xK_e ),  spawn "sublime_text"         )
              ,(( 0, xK_r ),  spawn "steam"                )
              ])
            , ((modm                    , xK_p          ), shellPrompt promptConfig )   
            , ((modm  .|. shiftMask     , xK_p          ), passPrompt promptConfig  )
            ]
