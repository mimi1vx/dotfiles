-- Ondřej Súkup xmonad config ...
-- main import
import XMonad
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
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce                    (spawnOnce)
import XMonad.Util.Themes
--xmonad layouts
import XMonad.Layout.Fullscreen 
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
-- import xmonad promt
import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
-- qualified imports of Data and Stack
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

promptConfig = defaultXPConfig
        { font        = "xft:Source Code Pro:pixelsize=12"
        , borderColor = "#1e2320"
--       , fgColor     = "#dddddd"
--        , fgHLight    = "#ffffff"
--        , bgColor     = "#1e2320"
--        , bgHLight    = "#5f5f5f"
        , height      = 18
        , position    = Top
        }
------------------------------------------------------------------------
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["con","web","irc","sublime","steam"] ++ map show [6 .. 9]

------------------------------------------------------------------------
-- add mouse buttons
button8 = 8 :: Button
button9 = 9 :: Button

-----------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ smartBorders $ layoutHints 
    $ onWorkspace "con" ( tab ||| tiled )
    $ onWorkspaces ["web","irc","steam"]  full 
    $ onWorkspace "sublime" ( full ||| tiled )
    $ full ||| tab ||| tiled
      where
          -- default tiling algorithm partitions the screen into two panes
          tiled   = Tall nmaster delta ratio

          -- The default number of windows in the master pane
          nmaster = 1

          -- Default proportion of screen occupied by master pane
          ratio   = toRational(2/(1+sqrt 5::Double))

          -- Percent of screen to increment by when resizing panes
          delta   = 5/100

          -- tab is tabbed
          tab     = tabbed shrinkText ( theme smallClean )

          -- full is Full
          full    = fullscreenFloat Full
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
myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> hintsEventHook
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
main = do 
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh defaults {
    	logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmproc }
    	} `removeKeys` 
            [ (mod4Mask .|. m, k) | (m, k) <- zip [0, shiftMask] [xK_w, xK_e, xK_r,xK_p] ]
          `additionalKeys`
            [ ((mod4Mask                    , xK_g          ), goToSelected defaultGSConfig                       )
            , ((mod4Mask                    , xK_Print      ), spawn "scrot '%F-%H-%M-%S.png' -e 'mv $f ~/Shot/'" )
            , ((mod4Mask                    , xK_s          ), scratchpadSpawnAction defaults                     )                
            , ((mod4Mask  .|. controlMask   , xK_p          ), submap . M.fromList $
              [(( 0, xK_q ),  spawn "quasselclient"        )
              ,(( 0, xK_w ),  spawn "google-chrome-stable" )
              ,(( 0, xK_e ),  spawn "sublime_text"         )
              ,(( 0, xK_r ),  spawn "steam"                )
              ])
            , ((mod4Mask                    , xK_p          ), shellPrompt promptConfig )   
            , ((mod4Mask  .|. shiftMask     , xK_p          ), passPrompt promptConfig  )
            ]
          `additionalMouseBindings`
            [ ((0,         button8), const prevWS )
            , ((0,         button9), const nextWS )
            ]



defaults = defaultConfig {
    terminal           = "urxvtc",
    --focusFollowsMouse  = myFocusFollowsMouse,
    --clickJustFocuses   = myClickJustFocuses,
    borderWidth        = 2,
    modMask            = mod4Mask,
    workspaces         = myWorkspaces,
    --normalBorderColor  = myNormalBorderColor,
    --focusedBorderColor = myFocusedBorderColor,
    layoutHook         = myLayout,      
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    startupHook        = myStartupHook
    }
