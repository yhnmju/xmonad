import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
-- import XMonad.Actions.Navigation2D -- switch to this once new version of XMonad's available
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

import Control.Monad (liftM2)

main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey (ewmh myConfig)

normalBorderCol, focusedBorderCol, currentCol, layoutCol :: String
currentCol       = "#fff"
layoutCol        = "#aaa"
normalBorderCol  = "#000"
focusedBorderCol = "#4af" -- and current window's title in status bar

-- what's displayed in the status bar
myPP :: PP
myPP = defaultPP
        { ppCurrent = xmobarColor currentCol "" . wrap "[" "]"
        , ppLayout = xmobarColor layoutCol ""
        , ppTitle = xmobarColor focusedBorderCol ""
        }

-- key binding to toggle the gap for the bar
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myManageHook = composeAll
        [ className =? "VirtualBox" --> viewShift "9:maxed"
        , manageDocks
        ] <+> manageHook defaultConfig
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

tiledLayout = avoidStruts $
        smartBorders $
        windowNavigation $
        renamed [Replace "Tall"] (rTall) ||| renamed [Replace "Wide"] (Mirror rTall)
    where
        rTall = ResizableTall 1 (1/16) (1/2) []

fullscreenLayout = avoidStruts $
        smartBorders $
        Full

myLayoutHook = onWorkspace "9:maxed" fullscreenLayout $ tiledLayout

-- startup applications - these are run each time XMonad is (re)started.
myStartupHook = do
        spawn "xrdb -merge ${HOME}/.Xresources"
        spawn "xsetroot -cursor_name left_ptr"
        spawn "/usr/bin/feh --bg-fill /usr/share/backgrounds/f31/extras/descent-to-loch-ericht.png"
  --      spawn "xscreensaver"

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9:maxed"]

keysToMoveWindowAndSwitchWorkspaces =
        [ ("M-"++m++show k, f i)
        | (i, k) <- zip myWorkspaces ([1 .. 9]++[0])
        , (f, m) <-
                [ (windows . W.greedyView, ""),
                  (\i -> windows (W.shift i) >> windows (W.greedyView i), "S-")
                ]
        ]

-- main configuration
myConfig = defaultConfig
        { manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , modMask            = mod4Mask -- Rebind Mod to the Windows key
        , borderWidth        = 1
        , normalBorderColor  = normalBorderCol
        , focusedBorderColor = focusedBorderCol
        , workspaces         = myWorkspaces
        , startupHook        = myStartupHook
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_h), sendMessage MirrorShrink)
        , ((mod4Mask .|. shiftMask, xK_l), sendMessage MirrorExpand)
        , ((mod4Mask,               xK_Right), sendMessage $ Go R)
        , ((mod4Mask,               xK_Left ), sendMessage $ Go L)
        , ((mod4Mask,               xK_Up   ), sendMessage $ Go U)
        , ((mod4Mask,               xK_Down ), sendMessage $ Go D)
        , ((mod4Mask .|. shiftMask, xK_Right), sendMessage $ Swap R)
        , ((mod4Mask .|. shiftMask, xK_Left ), sendMessage $ Swap L)
        , ((mod4Mask .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
        , ((mod4Mask .|. shiftMask, xK_Down ), sendMessage $ Swap D)
        , ((0,                      0x1008ff03), spawn "dimmer")
        , ((0,                      0x1008ff02), spawn "brighter")
        , ((0,                      0x1008ff11), spawn "amixer -q set Master 5%- unmute")
        , ((0,                      0x1008ff13), spawn "amixer -q set Master 5%+ unmute")
        , ((0,                      0x1008ff12), spawn "amixer -q set Master toggle")
 --       , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
 --       , ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command -l")
        ]
        `additionalKeysP`
        keysToMoveWindowAndSwitchWorkspaces

