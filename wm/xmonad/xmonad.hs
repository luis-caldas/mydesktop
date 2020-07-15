{-# LANGUAGE FlexibleContexts #-}
-- {{{ Imports

-- XMonad
import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run

-- System
import System.Environment

-- Data
import Data.List

import Data.Maybe

import qualified Data.Text
import qualified Data.Map

-- Window managing
import qualified XMonad.StackSet

-- }}}
-- {{{ Configs

-- Scaling
scalingVarName = "GDK_SCALE"

-- XResources
xrCommand               = ("xrdb", ["-query"])

xrVarBorder             = "xmonad.border"
xrVarSpace              = "xmonad.space"
xrVarBorderColour       = "xmonad.border-colour"
xrVarBorderColourActive = "xmonad.border-colour-active" 
xrVarBarBack            = "xmobar.background"
xrVarBarFore            = "xmobar.foreground"

-- Font
myFontSize = 12
myFontFace = "mono"

-- My applications
myTerminal = "APPLICATION_UNICODE=true st -f \"" ++ myFontFace ++ ":size=" ++ (show myFontSize) ++ "\""
myBrowser  = "firefox"
myLauncher = "neorofi"
myExplorer = "nautilus"
myMail     = "thunderbird"
myPrint    = "neoscrot"
myPrintSel = "neoscrot select"
myClip     = "neoclip"

-- My borders
myBorderWidth = 2

myFocusedBorderColour = "#FFFFFF"
myNormalBorderColour  = "#000000"

-- Spacing
myWindowSpacing = 5

-- Steps for changing volume/backlight
myKeyStep = 5

-- Wokspaces
myWorkspaces = map show [ 1 .. 9 ]

-- Mod key
myModKey = mod1Mask -- alt

-- XMobar config files
displayVar = "DISPLAY"

myBar = "xmobar"

myBarDefaultBack = "#FFFFFF"
myBarDefaultFore = "#000000"

myBarArguments :: Integer -> [String]
myBarArguments scalingFactor =
    [ "-f", "xft:" ++ myFontFace ++ ":size=" ++ (show myFontSize)
    ]

myBarColourArguments :: String -> String -> [String]
myBarColourArguments foreColour backColour =
    [ "-F", foreColour, "-B", backColour]

myBarConfigs = ( myBarConfigFolder ++ "/top.xmobarrc"
               , myBarConfigFolder ++ "/bottom.xmobarrc"
               ) where
                    myBarConfigFolder = "\"${HOME}\"/.config/xmobar"

myBarPP = def { ppCurrent         = wrap "[" "]"
              , ppSep             = "] ["
              , ppHidden          = wrap "|" "|"
              , ppHiddenNoWindows = wrap " " " "
              , ppLayout          = const ""
              , ppTitle           = shorten 50
              } 

-- Commands that should be run before startup
myStartupCommands = [ -- Cursor setting
                      "xsetroot -cursor_name left_ptr"
                    ]
-- }}}
-- {{{ Keybindings

-- Launching
myKeyBindings = [ 
                -- Spawners
                  ("M-<Return>", spawn myTerminal)
                , ("M-u"       , spawn myClip)
                , ("M-n"       , spawn myBrowser)
                , ("M-<Space>" , spawn myLauncher)
                -- Killer
                , ("M-<Backspace>", kill)
                -- Navigation
                , ("M-["  , windows XMonad.StackSet.focusUp)
                , ("M-/"  , windows XMonad.StackSet.focusDown)
                , ("M-p"  , windows XMonad.StackSet.swapMaster)
                , ("M-S-[", windows XMonad.StackSet.swapUp)
                , ("M-S-/", windows XMonad.StackSet.swapDown)
                -- Resizing
                , ("M-'", sendMessage Expand)
                , ("M-;", sendMessage Shrink)
                -- Volume
                , ("<XF86AudioMute>"       , spawn "pactl set-sink-mute   @DEFAULT_SINK@ toggle")
                , ("<XF86AudioLowerVolume>", spawn ("pactl set-sink-volume @DEFAULT_SINK@ -" ++ (show myKeyStep) ++ "%"))
                , ("<XF86AudioRaiseVolume>", spawn ("pactl set-sink-volume @DEFAULT_SINK@ +" ++ (show myKeyStep) ++ "%"))
                -- Backlight
                , ("<XF86MonBrightnessUp>"  , spawn ("light -A " ++ (show myKeyStep)))
                , ("<XF86MonBrightnessDown>", spawn ("light -U " ++ (show myKeyStep)))
                -- Audio Media
                , ("<XF86AudioPlay>"         , spawn "playerctl play-pause")
                , ("<XF86AudioNext>"         , spawn "playerctl next")
                , ("<XF86AudioPrev>"         , spawn "playerctl previous")
                , ("<XF86AudioStop>"         , spawn "playerctl stop")
                , ("M-<XF86AudioMute>"       , spawn "playerctl position 0")
                , ("M-<XF86AudioLowerVolume>", spawn ("playerctl position -" ++ (show myKeyStep)))
                , ("M-<XF86AudioRaiseVolume>", spawn ("playerctl position +" ++ (show myKeyStep)))
                -- Other Media
                , ("<XF86Explorer>", spawn myExplorer)
                , ("<XF86Mail>"    , spawn myMail)
                , ("<Print>"       , spawn myPrint)
                , ("M-<Print>"     , spawn myPrintSel)
                -- Lock
                , ("M-S-l", spawn "loginctl lock-session")
                ] ++
                -- Displays shortcut
                [ (("M" ++ shift ++ key), screenWorkspace sc >>= flip whenJust (windows . f))
                    | (key, sc) <- zip (map ("-"++) ["q", "w", "e", "r"]) [0..]
                    , (f, shift) <- [ (XMonad.StackSet.view, "")
                                    , (\f -> XMonad.StackSet.view f . XMonad.StackSet.shift f, "-S")
                                    ]] ++
                -- Workspaces shortcuts
                [ (("M" ++ shift ++ key), windows $ f i)
                    | (i, key) <- zip myWorkspaces (map ("-"++) (map show [1..9]))
                    , (f, shift) <- [ (XMonad.StackSet.greedyView, "")
                                    , (\i -> XMonad.StackSet.greedyView i . XMonad.StackSet.shift i, "-S")
                                    ]]

-- Bindings that should be removed
myRemoveBindings = [ "M-S-<Return>"
                 , "M-p"
                 , "M-S-c"
                 , "M-S-<Space>"
                 , "M-,"
                 , "M-."
                 , "M-S-/"
                 , "M-?"
                 ] ++ 
                 (map ("M-"++) ["h", "j", "k", "l"]) ++
                 [ "M-" ++ s ++ [n] 
                 | s <- ["", "S-"]
                 , n <- (['q', 'w', 'e', 'r'] ++ ['1' .. '9'])
                 ]

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

-- Adds double quotes to the beginning and end of a string
addQuotes :: String -> String
addQuotes stringIn =
    "\"" ++ stringIn ++ "\""

-- Extract tuple of ':' separate data from a line
extractTupleLine :: String -> (String, String)
extractTupleLine lineInput =
    (name, value) 
    where    
        fragmented = Data.Text.splitOn (Data.Text.pack ":") (Data.Text.pack lineInput)
        name = Data.Text.unpack $ Data.Text.strip $ head fragmented
        value = Data.Text.unpack $ Data.Text.strip $ fullString
            where
                fullString = Data.Text.pack $ intercalate " " $ map Data.Text.unpack (tail fragmented)

-- Transforms a xresources string into a xresources data
xrdbParse :: String -> Data.Map.Map String String
xrdbParse stringInput = 
    Data.Map.fromList dataList
    where 
        dataList = map extractTupleLine listLines
            where
                listLines = lines stringInput

-- Look data in a map and return it
lookMap :: Data.Map.Map String String -> String -> String -> String
lookMap mapInput mapSearch defaultVal =
    fromMaybe defaultVal lookedVar
    where 
        lookedVar = Data.Map.lookup mapSearch mapInput

-- Transforms string to integer
mToInteger :: String -> Integer
mToInteger strIn = 
    read strIn::Integer

-- }}}
-- {{{ Main

main = do
    -- Get the scaling of the session
    scalingRaw <- getEnv scalingVarName
    let scaling = mToInteger scalingRaw

    -- Get the display and the path for the selected display
    display <- getEnv displayVar 

    -- Extract all the xresource data
    xrdbString <- runProcessWithInput (fst xrCommand) (snd xrCommand) ""
    let xrdbData = xrdbParse xrdbString
    -- Extract each
    let xrBorder       = mToInteger $ lookMap xrdbData xrVarBorder (show myBorderWidth)
    let xrSpace        = mToInteger $ lookMap xrdbData xrVarSpace (show myWindowSpacing)
    let xrColour       = lookMap xrdbData xrVarBorderColour myNormalBorderColour
    let xrActiveColour = lookMap xrdbData xrVarBorderColourActive myFocusedBorderColour
    let xrBarBack      = lookMap xrdbData xrVarBarBack myBarDefaultBack
    let xrBarFore      = lookMap xrdbData xrVarBarFore myBarDefaultFore

    -- Create the bar command
    let myBarCommandTop = unwords [ myBar
                                  , fst myBarConfigs
                                  , argumentsToString $ myBarArguments scaling
                                  , argumentsToString $ myBarColourArguments (addQuotes xrBarFore) (addQuotes xrBarBack)
                                  ]
    let myBarCommandBottom = unwords [ myBar
                                     , snd myBarConfigs
                                     , argumentsToString $ myBarArguments scaling
                                     , argumentsToString $ myBarColourArguments (addQuotes xrBarFore) (addQuotes xrBarBack)
                                     ]

    -- Unsafe spawn top bar
    unsafeSpawn myBarCommandTop

    -- Run all the startup commands
    mapM_ unsafeSpawn myStartupCommands

    -- Main config
    let myDefaultConfig = def
            { modMask            = myModKey
            , borderWidth        = fromInteger $ scalePixels scaling xrBorder
            , normalBorderColor  = xrColour
            , focusedBorderColor = xrActiveColour
            , layoutHook         = spacingRawScalable xrSpace scaling $
                                   smartBorders $
                                   layoutHook def
            , manageHook         = manageDocks <+> manageHook def
            , handleEventHook    = handleEventHook def <+> fullscreenEventHook
            , startupHook        = startupHook def <+> setFullscreenSupported
            , workspaces         = myWorkspaces
            } 
  
    -- Add my key bindings
    xmobarSpawner <- spawnMyBar myBarCommandBottom myBarPP $ ewmh $ myDefaultConfig
                 `removeKeysP`     myRemoveBindings
                 `additionalKeysP` myKeyBindings

    -- Call xmonad
    xmonad $ xmobarSpawner

-- }}}
