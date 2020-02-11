import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


main = do
  xmonad =<< statusBar "xmobar" myPP toggleStrutsKey defaultConfig
    { terminal           = "xterm"
    , focusFollowsMouse  = True
    , clickJustFocuses   = False
    , borderWidth        = 1
    , modMask            = mod4Mask
--    , workspaces         = myworkspaces
    , normalBorderColor  = "#dddddd"
    , focusedBorderColor = "#00dd00"
    , manageHook         = mymanager
    }

myPP = xmobarPP { ppOutput          = putStrLn
                , ppCurrent         = xmobarColor "lightgreen" "" . wrap "[" "]"
                --, ppHiddenNoWindows = xmobarColor "grey" ""
                , ppTitle           = xmobarColor "lightgreen"  "" . shorten 20
                , ppLayout          = shorten 6
                , ppVisible         = wrap "(" ")"
                , ppUrgent          = xmobarColor "red" "yellow"
                }

toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

--myworkspaces = [ "code"
--              , "web"
--               , "media"
--               , "irc"
--               , "random"
--               , "mail"
--               , "docs"
--               , "music"
--               , "root"
--               ]

mymanager = composeAll
  [ className =? "gimp" --> doFloat
  , className =? "vlc"  --> doFloat
  ]
