-- {{{ Imports

import XMonad
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Cursor
import System.Environment

--- }}}

-- {{{ Functions

lastN :: Int -> [Char] -> [Char]
lastN n xs = drop (length xs - n) xs

isMainMonitor :: [Char] -> Bool
isMainMonitor display
    | lastChars == ":0" = True
    | lastChars == ".0" = True
    | otherwise = False
    where
        lastChars = lastN 2 display

getItemListBool :: Bool -> [[Char]] -> [Char]
getItemListBool boolIn listIn
    | boolIn    = listIn!!0
    | otherwise = listIn!!1

-- }}}

-- {{{ Config vars

-- Font
myFontSize = 10
myFontFace = "mono"

-- My applications
myTerminal = "APPLICATION_UNICODE=true " ++ " " ++
             "st -f \"" ++ myFontFace ++ ":size=" ++ (show myFontSize) ++ "\""
myBrowser = "surf"

-- My borders
myBorderWidth = 2
myFocusedBorderColour = "#a0a0a0"
myNormalBorderColour = "#404040"

-- Wokspaces
myWorkspaces = [ "!", "@", "#", "$" ]

-- Spacing between windows
myLayoutHook = avoidStruts $
               spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True $
               layoutHook defaultConfig

-- Startup hook
myStartupHook = setDefaultCursor xC_left_ptr

-- XMobar config files
displayVar = "DISPLAY"
myBar = "xmobar"
myBarConfigs = [ "\"${HOME}\"/.config/xmobar/mainrc"
               , "\"${HOME}\"/.config/xmobar/siderc"
               ]

-- Main config
myDefaultConfig = defaultConfig
        { terminal           = myTerminal
        , borderWidth        = myBorderWidth
        , focusedBorderColor = myFocusedBorderColour
        , normalBorderColor  = myNormalBorderColour
        , layoutHook         = myLayoutHook
        , manageHook         = manageHook defaultConfig <+> manageDocks
        , startupHook        = myStartupHook
        , workspaces         = myWorkspaces
        } 


--- }}}

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- {{{ Main
main = do
    -- Get the display and the path for the selected display
    display <- getEnv displayVar
    let xmobarPath = getItemListBool (isMainMonitor display) myBarConfigs
    let myBarCommand = myBar ++ " " ++ xmobarPath

    -- Configure the status bar
    -- statusBarConfigured <- statusBar myBarCommand myPP toggleStrutsKey myDefaultConfig

    -- Call xmonad
    xmonad $ docks myDefaultConfig
-- }}}
