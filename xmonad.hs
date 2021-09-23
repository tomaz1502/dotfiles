{-# LANGUAGE RecordWildCards #-}

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.FuzzyMatch
import qualified XMonad.StackSet as W

import           Data.Maybe (fromJust)
import qualified Data.Map as M
import           System.Exit

import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import System.IO
 

main :: IO ()
main = do
  xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myConfig = def
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = 2
    , normalBorderColor  = "#888888"
    , focusedBorderColor = "#77DD77"
    , focusFollowsMouse  = False
    , startupHook        = myStartupHook
    , keys               = myKeys
    , workspaces         = myWorkspaces
    , layoutHook         = mySpacing myLayoutHook
    , logHook            = dynamicLogWithPP xmobarPP
    }

-- Command to launch the bar.
myBar = "xmobar ~/.config/xmobar/xmobarrc"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "[" "]"
                , ppVisible = xmobarColor "#FFFFFF" "#000000" -- . clickable
                , ppOrder   = \(ws : _ : r) -> ws : r
                -- , ppHidden = xmobarColor "#000000" ""
                -- , ppHiddenNoWindows = xmobarColor "#000000" "" . clickable
                }



myWorkspaces = [ show i | i <- [1 .. 10] ]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_o)

mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True

myStartupHook :: X ()
myStartupHook =
    -- spawn "feh --bg-fill ~/Pictures/wallpapers/0058.jpg" >>
    spawn "feh --bg-fill ~/Pictures/calm.jpg" >>
    spawn "picom -b"

myFont :: String
myFont = "xft:Fira Code Nerd Font:regular:size=18"

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myModMask :: KeyMask
myModMask = mod4Mask -- Win key or Super_L

myLayoutHook = smartBorders tiled
           ||| smartBorders (Mirror tiled)
           ||| smartBorders Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {..}) = M.fromList $
    -- -- launching and killing programs
    [ ((modMask,               xK_q     ), kill)
    , ((modMask,               xK_Return), spawn terminal )
    , ((modMask,               xK_p     ), spawn "dmenu_run -fn \"Fira Mono-16\"")
    , ((modMask,               xK_b     ), spawn myBrowser)
    , ((modMask,               xK_e     ), spawn "emacs")
    , ((modMask,               xK_t     ), spawn "gnome-terminal")
    , ((modMask,               xK_c     ), spawn "telegram-desktop")
    , ((modMask,               xK_s     ), spawn "slack")
    , ((modMask,               xK_n     ), spawn "nautilus")
    , ((modMask              , xK_period), spawn "~/Desktop/dotfiles/Script/switch_keys.sh")
    , ((modMask              , xK_Print ), spawn "maim --format=png /dev/stdout | xclip -selection clipboard -t image/png -i")
    , ((modMask .|. shiftMask, xK_Print ), spawn "maim -s --format=png /dev/stdout | xclip -selection clipboard -t image/png -i")

    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ layoutHook)


    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  )
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    , ((modMask,               xK_m     ), windows W.focusMaster)

    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_r     ), confirmPrompt xpConfig "exit" $ io (exitWith ExitSuccess))
    , ((modMask              , xK_r     ), spawn respawnCommand)
    ]
    ++
    [ ((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  where
    respawnCommand =
      "xmonad --recompile; pkill xmobar; xmonad --restart"

xpConfig :: XPConfig
xpConfig = def
      { font     = myFont
      , position = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height   = 35
      }

