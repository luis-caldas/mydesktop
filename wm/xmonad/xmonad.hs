-- {{{ Imports

-- Local
import Mine.Defaults
import Mine.Utils

-- XMonad
import XMonad
-- import XMonad.Layout.NoBorders -- Still have to figure out how to remove borders on fullscreen
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run

-- System
import System.Environment

-- Data
import Data.List

-- }}}

-- {{{ Main

main = do
    -- Get the scaling of the session
    scalingRaw <- getEnv scalingVarName
    let scaling = mToInteger scalingRaw

    -- Get the display and the path for the selected display
    display <- getEnv displayVar 
    let myBarCommand = unwords [ myBar
                               , monitorConfig display myBarConfigs
                               , argumentsToString $ myBarArguments scaling
                               ]
    let myCompositorCommand = unwords [ myCompositor
                                      , argumentsToString $ myCompositorArguments scaling
                                      ]

    -- Extract all the xresource data
    xrdbString <- runProcessWithInput (fst xrCommand) (snd xrCommand) ""
    let xrdbData = xrdbParse xrdbString
    -- Extract each
    let xrBorder       = mToInteger $ lookMap xrdbData xrVarBorder (show myBorderWidth)
    let xrSpace        = mToInteger $ lookMap xrdbData xrVarSpace (show myWindowSpacing)
    let xrColour       = lookMap xrdbData xrVarBorderColour myNormalBorderColour
    let xrActiveColour = lookMap xrdbData xrVarBorderColourActive myFocusedBorderColour

    -- Run the window composer
    unsafeSpawn myCompositorCommand

    -- Run all the startup commands
    mapM_ unsafeSpawn myStartupCommands

    -- Main config
    let myDefaultConfig = def
            { modMask            = myModKey
            , borderWidth        = fromInteger $ scalePixels scaling xrBorder
            , normalBorderColor  = xrColour
            , focusedBorderColor = xrActiveColour
            , layoutHook         = spacingRawScalable xrSpace scaling $ 
                                   layoutHook def
            , manageHook         = manageDocks <+> manageHook def
            , handleEventHook    = handleEventHook def <+> fullscreenEventHook
            , startupHook        = startupHook def <+> setFullscreenSupported
            , workspaces         = myWorkspaces
            } 
  
    -- Add my key bindings
    xmobarSpawner <- spawnMyBar myBarCommand myBarPP $ ewmh $ myDefaultConfig
                 `removeKeysP`     myRemoveBindings
                 `additionalKeysP` myKeyBindings

    -- Call xmonad
    xmonad $ xmobarSpawner

-- }}}
