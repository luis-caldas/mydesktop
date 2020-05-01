{-# LANGUAGE FlexibleContexts #-}
-- {{{ Imports

-- XMonad
import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Operations

-- System
import System.Environment

-- }}}

-- {{{ Functions

-- Receives a DISPLAY string and returns one of the items of the 
-- given tuple
monitorConfig :: String -> (String, String) -> String
monitorConfig display displayConfigList
    | lastChars == ":0" = mainMonitorConfig
    | lastChars == ".0" = mainMonitorConfig
    | otherwise = sideMonitorConfig
    where
        lastChars = drop (length display - 2) display
        mainMonitorConfig = fst displayConfigList
        sideMonitorConfig = snd displayConfigList

-- Scales pixels with a multiplier
scalePixels :: Integer -> Integer -> Integer
scalePixels scale inputPx =
    scale * inputPx

-- Creates a spacing that is scalable
spacingRawScalable :: Integer -> Integer -> l a -> ModifiedLayout Spacing l a
spacingRawScalable borderPx scalingFactor = 
    spacingRaw False borderScaled True borderScaled True
    where
        borderSizeScaled = scalePixels scalingFactor borderPx
        borderScaled = Border borderSizeScaled borderSizeScaled borderSizeScaled borderSizeScaled

-- Spawn xmobar with input pipe
spawnMyBar :: LayoutClass l Window => String -> PP -> XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
spawnMyBar commandString ppIn confIn = do
    pipe <- spawnPipe commandString
    return $ docks $ confIn 
        { layoutHook = avoidStruts (layoutHook confIn)
        , logHook    = do
                        logHook confIn
                        dynamicLogWithPP ppIn { ppOutput = hPutStrLn pipe }
        }

-- }}}

-- {{{ Config vars

-- Font
myFontSize = 10
myFontFace = "mono"

-- My applications
myTerminal = "APPLICATION_UNICODE=true st -f \"" ++ myFontFace ++ ":size=" ++ (show myFontSize) ++ "\""
myBrowser = "surf"
myLauncher = "neorofi"

-- My borders
myBorderWidth = 2
myFocusedBorderColour = "#a0a0a0"
myNormalBorderColour = "#404040"

-- Wokspaces
myWorkspaces = [ "!", "@", "#", "$" ]

-- Mod key
myModKey = mod1Mask -- alt

-- Spacing between windows
-- myLayoutHook = avoidStruts $

-- XMobar config files
displayVar = "DISPLAY"
myBar = "xmobar"
myBarConfigs = ( myBarConfigFolder ++ "/mainrc"
               , myBarConfigFolder ++ "/siderc"
               ) where
                    myBarConfigFolder = "\"${HOME}\"/.config/xmobar/"
myBarPP = def { ppCurrent         = wrap "[" "]"
              , ppSep             = " | "
              , ppHidden          = wrap " " "."
              , ppHiddenNoWindows = wrap " " " "
              , ppLayout          = const ""
              , ppTitle           = shorten 70
              } 

-- Commands that should be run before startup
myStartupCommands = [ 
                      -- Cursor setting
                      "xsetroot -cursor_name left_ptr"
                    ]
-- }}}

-- {{{ Keybindings

-- Launching
myKeyBindings = [ ("M-<Return>"   , spawn myTerminal)
                , ("M-n"          , spawn myBrowser)
                , ("M-<Backspace>", kill)
                ]

-- Bindings that should be removed
myRemoveBindings = [ "M-S-<Return>"
                 , "M-p"
                 , "M-S-c"
                 , "M-S-<Space>"
                 , "M-,"
                 , "M-."
                 , "M-q"
                 , "M-S-q"
                 , "M-S-/"
                 , "M-?"
                 ] ++ 
                 ["M-S-" ++ [n] | n <- ['1'..'9']] ++
                 ["M-S-" ++ n | n <- ["w","e","r"]]

-- }}}

-- {{{ Main

main = do
    -- Get the display and the path for the selected display
    display <- getEnv displayVar 
    let myBarCommand = myBar ++ " " ++ (monitorConfig display myBarConfigs) ++
                       " " ++ "-f" ++ " " ++ "xft:" ++ myFontFace ++ ":size=" ++ show(myFontSize)

   
    -- Get the scaling of the session
    scalingRaw <- getEnv "GDK_SCALE"
    let scaling = read scalingRaw::Integer

    -- Run all the startup commands
    mapM_ unsafeSpawn myStartupCommands

    -- Main config
    let myDefaultConfig = defaultConfig
            { modMask            = myModKey
            , borderWidth        = fromInteger $ scalePixels scaling myBorderWidth
            , focusedBorderColor = myFocusedBorderColour
            , normalBorderColor  = myNormalBorderColour
            , layoutHook         = spacingRawScalable 10 scaling $ 
                                   layoutHook def
            , manageHook         = manageHook defaultConfig <+> manageDocks
            , workspaces         = myWorkspaces
            } 
  
    -- Add my key bindings
    xmobarSpawner <- spawnMyBar myBarCommand myBarPP $ myDefaultConfig
                 `additionalKeysP` myKeyBindings
                 `removeKeysP`     myRemoveBindings

    -- Call xmonad
    xmonad $ xmobarSpawner

-- }}}
