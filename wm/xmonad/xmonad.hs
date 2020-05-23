{-# LANGUAGE FlexibleContexts #-}
-- {{{ Imports

-- XMonad
import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Operations

-- Window focus
import qualified XMonad.StackSet as W

-- System
import System.Environment

-- Data
import Data.List

-- }}}

-- {{{ Functions

-- Hack to let firefox fullscreen
setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_FULLSCREEN" -- XXX Copy-pasted to add this line
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

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

-- Transforms a list of arguments into a command string
argumentsToString :: [String] -> String
argumentsToString argsList = 
    intercalate " " argsList

-- }}}

-- {{{ Config vars

-- Font
myFontSize = 10
myFontFace = "mono"

-- My applications
myTerminal = "APPLICATION_UNICODE=true st -f \"" ++ myFontFace ++ ":size=" ++ (show myFontSize) ++ "\""
myBrowser = "firefox"
myLauncher = "neorofi"

-- My borders
myBorderWidth = 2
myFocusedBorderColour = "#a0a0a0"
myNormalBorderColour = "#404040"

-- Wokspaces
myWorkspaces = map show [ 1 .. 9 ]

-- Mod key
myModKey = mod1Mask -- alt

-- Spacing between windows
-- myLayoutHook = avoidStruts $

-- XMobar config files
displayVar = "DISPLAY"
myBar = "xmobar"
myBarArguments :: Integer -> [String]
myBarArguments scalingFactor =
    [ "-f", "xft:" ++ myFontFace ++ ":size=" ++ (show myFontSize)
    ]
myBarConfigs = ( myBarConfigFolder ++ "/main.xmobarrc"
               , myBarConfigFolder ++ "/side.xmobarrc"
               ) where
                    myBarConfigFolder = "\"${HOME}\"/.config/xmobar"
myBarPP = def { ppCurrent         = wrap "[" "]"
              , ppSep             = " | "
              , ppHidden          = wrap " " "."
              , ppHiddenNoWindows = wrap " " " "
              , ppLayout          = const ""
              , ppTitle           = shorten 70
              } 

-- Picom config
myCompositor = "picom"
myCompositorArguments :: Integer -> [String]
myCompositorArguments scalingFactor = 
    [ "--shadow"
    , "--shadow-opacity" , show 0.5
    , "--shadow-radius"  , show $ scalingFactor * 10
    , "--shadow-offset-x", show $ scalingFactor * 10
    , "--shadow-offset-y", show $ scalingFactor * 10
    , "--no-dock-shadow"
    , "--fading"
    , "--fade-delta"     , show 5
    ]


-- Commands that should be run before startup
myStartupCommands = [ 
                      -- Cursor setting
                      "xsetroot -cursor_name left_ptr"
                    ]
-- }}}

-- {{{ Keybindings

-- Launching
myKeyBindings = [ 
                -- Spawners
                  ("M-<Return>"   , spawn myTerminal)
                , ("M-n"          , spawn myBrowser)
                , ("M-<Space>"    , spawn myLauncher)
                -- Killer
                , ("M-<Backspace>", kill)
                -- Navigation
                , ("M-["          , windows W.focusUp)
                , ("M-/"          , windows W.focusDown)
                , ("M-p"          , windows W.swapMaster)
                , ("M-S-["        , windows W.swapUp)
                , ("M-S-/"        , windows W.swapDown)
                -- Resizing
                , ("M-'"          , sendMessage Expand)
                , ("M-;"          , sendMessage Shrink)
                ] ++ 
                -- Displays shortcut
                [ (("M" ++ shift ++ key), screenWorkspace sc >>= flip whenJust (windows . f))
                    | (key, sc) <- zip (map ("-"++) ["q", "w", "e", "r"]) [0..]
                    , (f, shift) <- [ (W.view, "")
                                    , (\f -> W.view f . W.shift f, "-S")
                                    ]] ++
                -- Workspaces shortcuts
                [ (("M" ++ shift ++ key), windows $ f i)
                    | (i, key) <- zip myWorkspaces (map ("-"++) (map show [1..9]))
                    , (f, shift) <- [ (W.greedyView, "")
                                    , (\i -> W.greedyView i . W.shift i, "-S")
                                    ]]

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
                 (map ("M-"++) ["h", "j", "k", "l"]) ++
                 ["M-" ++ n | n <- ["w", "e", "r"]] ++
                 ["M-" ++ [n] | n <- ['1'..'9']] ++
                 ["M-S-" ++ n | n <- ["w", "e", "r"]] ++
                 ["M-S-" ++ [n] | n <- ['1'..'9']]

-- }}}

-- {{{ Main

main = do
    -- Get the scaling of the session
    scalingRaw <- getEnv "GDK_SCALE"
    let scaling = read scalingRaw::Integer

    -- Get the display and the path for the selected display
    display <- getEnv displayVar 
    let myBarCommand = myBar ++ " " ++ (monitorConfig display myBarConfigs) ++
                       " " ++ (argumentsToString $ myBarArguments scaling)
    let myCompositorCommand = myCompositor ++ " " ++ (argumentsToString $ myCompositorArguments scaling)

    -- Run the window composer
    unsafeSpawn myCompositorCommand

    -- Run all the startup commands
    mapM_ unsafeSpawn myStartupCommands

    -- Main config
    let myDefaultConfig = def
            { modMask            = myModKey
            , borderWidth        = fromInteger $ scalePixels scaling myBorderWidth
            , focusedBorderColor = myFocusedBorderColour
            , normalBorderColor  = myNormalBorderColour
            , layoutHook         = smartBorders $ spacingRawScalable 10 scaling $ 
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
