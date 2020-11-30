-- Imports

import Data.Char
import Data.Maybe (isJust)
import Data.Tree

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.ManageDocks
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import qualified XMonad.Actions.Search as S
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"
myFont = "xft:Source Code Pro:regular:size=12"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--


myWorkspaces    = ["web","term","emacs","misc","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#666666"
myFocusedBorderColor = "#aaaaaa"


-- search engines:
google     = S.searchEngine "Google"    "https://news.google.com/search?q="
rust       = S.searchEngine "Rust Docs" "https://doc.rust-lang.org/std/index.html?search="
hoogle     = S.searchEngine "Hoogle" "https://hoogle.haskell.org/?hoogle="
cpp        = S.searchEngine "C++ Ref" "https://www.cplusplus.com/search.do?q="
arch       = S.searchEngine "Arch Wiki" "https://wiki.archlinux.org/index.php?search="
youtube    = S.searchEngine "Youtube" "https://www.youtube.com/results?search_query="

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm               , xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    -- , ((modm,               xK_r     ), spawn "dmenu_run_history -h 25 -nb '#000000' -nf '#FFFFFF' -sb '#00008B' -fn 'Source Code Pro:style=Regular 13' -p 'Exec' -S")

    , ((modm               , xK_r), shellPrompt dtXPConfig)

    , ((modm .|. shiftMask , xK_m), manPrompt dtXPConfig)
    
    , ((modm .|. shiftMask , xK_g), S.promptSearch dtXPConfig google)

    , ((modm .|. shiftMask , xK_r), S.promptSearch dtXPConfig rust)

    , ((modm .|. shiftMask , xK_h), S.promptSearch dtXPConfig hoogle)

    , ((modm .|. shiftMask , xK_c), S.promptSearch dtXPConfig cpp)

    , ((modm .|. shiftMask , xK_a), S.promptSearch dtXPConfig arch)

    , ((modm .|. shiftMask , xK_y), S.promptSearch dtXPConfig youtube)


    -- close focused window
    , ((modm              , xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    -- , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_v     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm , xK_minus), spawn "xmonad --recompile; xmonad --restart")

    -- Launch Chromium
    , ((modm , xK_b), spawn "chromium-browser")

    -- Launch Nautilus
    , ((modm , xK_f), spawn "nautilus")

    -- Launch Telegram
    , ((modm , xK_c), spawn "telegram-desktop")

    -- Modify keyboard layout
    , ((modm ,              xK_m), spawn "~/Tools/Script/switch_keys.sh")


    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
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
-- myLayout = spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $

-- myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)

mySpacing = spacingRaw False (Border 8 8 8 8) True (Border 8 8 8 8) True

myLayout = avoidStruts (tiled ||| noBorders Full)
  where
     tiled     = renamed [Replace "Tiled"]
                $ mySpacing
                $ ResizableTall 1 (3/100) (1/2) []

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
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

-- manageHook = manageDocks <+> manageHook def
------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn (spawnPipe "xmobar /home/tomaz1502/.config/xmobar/xmobar.config") }

-- myLogHook = dynamicLockWithPP $ def { ppOutput = hPutStrLn xmproc }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawnOnce "feh --bg-fill /home/tomaz1502/Pictures/rocket.png"
    spawnOnce "picom --experimental-backends"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
--
main = do
    h <- spawnPipe "xmobar /home/tomaz1502/.config/xmobar/xmobar.config"
    xmonad $ docks defaults { logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn h } }

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        -- logHook            = myLogHook,
        startupHook        = myStartupHook
    }

altMask = mod1Mask

dtXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
dtXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

dtXPConfig :: XPConfig
dtXPConfig = def
      { font                = myFont
      , bgColor             = "#282c34"
      , fgColor             = "#bbc2cf"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      -- , bgColor             = "#000000"
      -- , fgColor             = "#FFFFFF"
      -- , bgHLight            = "#00008B"
      -- , fgHLight            = "#FFFFFF"
      -- , borderColor         = "#00008B"
      , promptBorderWidth   = 0
      , promptKeymap        = dtXPKeymap
      , position            = Top
     -- , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 25
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = ""
      , autoComplete        = Nothing  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , defaultPrompter     = id  -- change prompt to UPPER
      -- , defaultPrompter     = unwords . map reverse . words  -- reverse the prompt
      -- , defaultPrompter     = drop 5 .id (++ "XXXX: ")  -- drop first 5 chars of prompt and add XXXX:
      , alwaysHighlight     = True
      , maxComplRows        = Just 7      -- set to 'Just 5' for 5 rows
      }



-- data DockToggleTime = DTT { lastTime :: Time } deriving (Eq, Show, Typeable)

-- instance ExtensionClass DockToggleTime where
--     initialValue = DTT 0

-- toggleDocksHook :: Int -> KeySym -> Event -> X All
-- toggleDocksHook to ks ( KeyEvent { ev_event_display = d
--                                 , ev_event_type    = et
--                                 , ev_keycode       = ekc
--                                 , ev_time          = etime
--                                 } ) =
--         io (keysymToKeycode d ks) >>= toggleDocks >> return (All True)
--     where
--     toggleDocks kc
--         | ekc == kc && et == keyPress = do
--             safeSendSignal ["Reveal 0", "TogglePersistent"]
--             XS.put ( DTT etime )
--         | ekc == kc && et == keyRelease = do
--             gap <- XS.gets ( (-) etime . lastTime )
--             safeSendSignal [ "TogglePersistent"
--                         , "Hide " ++ show (if gap < 400 then to else 0)
--                         ]
--         | otherwise = return ()

--     safeSendSignal s = catchX (io $ sendSignal s) (return ())
--     sendSignal    = withSession . callSignal
--     withSession mc = connectSession >>= \c -> callNoReply c mc >> disconnect c
--     callSignal :: [String] -> MethodCall
--     callSignal s = ( methodCall
--                     ( objectPath_    "/org/Xmobar/Control" )
--                     ( interfaceName_ "org.Xmobar.Control"  )
--                     ( memberName_    "SendSignal"          )
--                 ) { methodCallDestination = Just $ busName_ "org.Xmobar.Control"
--                     , methodCallBody        = map toVariant s
--                     }

-- toggleDocksHook _ _ _ = return (All True)

-- myDocksEventHook :: Event -> X All
-- myDocksEventHook e = do
--     when (et == mapNotify || et == unmapNotify) $
--         whenX ((not `fmap` (isClient w)) <&&> runQuery checkDock w) refresh
--     return (All True)
--     where w  = ev_window e
--           et = ev_event_type e

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
