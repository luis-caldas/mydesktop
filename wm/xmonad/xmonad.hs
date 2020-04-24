-- imports
import XMonad
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks

import XMonad.Util.Run(spawnPipe)

-- general config
myFontSize = 10
myFontFace = "mono"


-- my applications
myTerminal = "APPLICATION_UNICODE=true TMUX_START=true" ++ " " ++
             "st -f \"" ++ myFontFace ++ ":size=" ++ (show myFontSize) ++ "\""
myBrowser = "surf"

-- my borders
myBorderWidth = 4
myFocusedBorderColour = "#d0d0d0"
myNormalBorderColour = "#a0a0a0"

-- wokspaces
myWorkspaces = [ " 零 ", "一つ", "二つ", "三つ",
                 "四つ", "五つ", "六つ", "七つ",
                 "八つ", "九つ"
               ]

-- spacing between windows
myLayoutHook = spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True $
               layoutHook defaultConfig

-- main
main = xmonad $ defaultConfig
        { terminal           = myTerminal
        , borderWidth        = myBorderWidth
        , focusedBorderColor = myFocusedBorderColour
        , normalBorderColor  = myNormalBorderColour
        , layoutHook         = myLayoutHook
        , manageHook         = manageDocks <+> manageHook defaultConfig
        , workspaces         = myWorkspaces
        }


