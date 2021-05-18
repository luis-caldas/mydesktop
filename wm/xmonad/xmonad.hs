-- vim: set tabstop=8 softtabstop=0 expandtab shiftwidth=8 smarttab:
{-# LANGUAGE FlexibleContexts #-}
-- {{{ Imports

-- XMonad
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedWindows
import XMonad.Util.EZConfig
import XMonad.Util.Run

-- System
import System.Environment

-- Codec
import Codec.Binary.UTF8.String (encodeString)

-- Monad
import Control.Monad (liftM2)

-- Data
import Data.List

import Data.Maybe

import qualified Data.Text
import qualified Data.Map

-- Window managing
import qualified XMonad.StackSet

-- }}}
-- {{{ Configs

-- Env
-- Environment vars that should be set
envVarsSet = [ ("APPLICATION_UNICODE", "true")
             ]

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
myTerminal = "st"
myBrowser  = "firefox"
myLauncher = "neorofi"
myExplorer = "nautilus"
myMail     = "thunderbird"
myPrint    = "neoscrot"
myPrintSel = "neoscrot select"
myClip     = "neoclip"

-- Browser flags
myBrowserPersistentFlags = "-P persistent"
myBrowserBasicFlags      = "-P basic"

-- Floating programs and how they should float
myFloatingPrograms = [ ("neocalendar", doFloatAt 0.05 0.05)
                     , ("neoweather", doFloatAt 0.05 0.05)
                     , ("neosysinfo", doFloatAt 0.05 0.05)
                     , ("neobatt", doFloatAt 0.05 0.05)
                     , ("neonet", doFloatAt 0.05 0.05)
                     , ("glxgears", doCenterFloat)
                     ]

myTerminalArgs :: String -> [String]
myTerminalArgs programName =
    [
      programName,
      "-f" ++ myFontFace ++ ":size=" ++ (show myFontSize)
    ]

-- My borders
myBorderWidth = 2

myFocusedBorderColour = "#FFFFFF"
myNormalBorderColour  = "#000000"

-- Spacing
myWindowSpacing = 5

-- Steps for changing volume/backlight
myKeyStep = 2

-- Wokspaces
myWorkspaces = map show [ 1 .. 9 ]

-- Mod key
myModKey = mod1Mask -- alt

-- XMobar config files
displayVar = "DISPLAY"

myBar = "xmobar"

myBarDefaultBack = "#FFFFFF"
myBarDefaultFore = "#000000"

myBarArguments :: [String]
myBarArguments =
    [ "-f", "xft:" ++ myFontFace ++ ":size=" ++ (show myFontSize)
    ]

myBarColourArguments :: String -> String -> [String]
myBarColourArguments foreColour backColour =
    [ "-F", foreColour, "-B", backColour]

myBarConfigs = ( myBarConfigFolder ++ "/top.xmobarrc"
               , myBarConfigFolder ++ "/bottom.xmobarrc"
               ) where
                    myBarConfigFolder = "\"${HOME}\"/.config/xmobar"

myBarPP = def { ppCurrent          = wrap ">" ""
              , ppHidden           = wrap " " ""
              , ppHiddenNoWindows  = wrap " " ""
              , ppUrgent           = wrap "*" ""
              , ppSep              = "] ["
              , ppLayout           = const ""
              } 

-- Commands that should be run before startup
myStartupCommands = [ -- Cursor setting
                      "xsetroot -cursor_name left_ptr"
                    ]

-- Application starting layout
myApplicationStartLayouts = [ ( "M-o", do
                                         spawnOn (myWorkspaces!!0) $ argumentsToString $ myTerminalArgs myTerminal
                                         spawnOn (myWorkspaces!!1) myBrowser
                                         spawnOn (myWorkspaces!!2) myMail
                                         spawnOn (myWorkspaces!!7) $ argumentsToString $ [ myBrowser, myBrowserPersistentFlags ]
                                         spawnOn (myWorkspaces!!8) $ argumentsToString $ myTerminalArgs myTerminal
                              )
                            , ( "M-i", do
                                         spawnOn (myWorkspaces!!1) myBrowser
                                         spawnOn (myWorkspaces!!2) myMail
                                         spawnOn (myWorkspaces!!7) $ argumentsToString $ [ myBrowser, myBrowserPersistentFlags ]
                              )
                            ]

-- }}}
-- {{{ Keybindings

-- Launching
myKeyBindings = [ 
                -- Spawners
                  ("M-<Return>", spawn $ argumentsToString $ myTerminalArgs $ myTerminal)
                , ("M-u"       , spawn myClip)
                , ("M-n"       , spawn myBrowser)
                , ("M-m"       , spawn $ argumentsToString $ [ myBrowser, myBrowserPersistentFlags ])
                , ("M-b"       , spawn $ argumentsToString $ [ myBrowser, myBrowserBasicFlags ])
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
                                    ]] ++
                -- Multiple applications start shortcuts
                myApplicationStartLayouts

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

-- Floating windows of name when launched
myManageHook = composeAll $
               [
                 title =? (fst programTuple) --> (snd programTuple) |
                 programTuple <- myFloatingPrograms
               ]
               ++ [ manageDocks, manageSpawn ]

-- }}}
-- {{{ Functions

-- {{{ My PP functions

-- My reimplementation of XMonad.Hooks.DynamicLog


-- | Output a list of strings, ignoring empty ones and separating the
--   rest with the given separator.
mySepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
mySepBy sep = concat . intersperse sep . filter (not . null)

-- My dynamic log
myDynamicLog :: X ()
myDynamicLog = myDynamicLogWithPP def

myDynamicLogWithPP :: PP -> X ()
myDynamicLogWithPP pp = myDynamicLogString pp >>= io . ppOutput pp

myDynamicLogString :: PP -> X String
myDynamicLogString pp = do

    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp

    let ld = description . XMonad.StackSet.layout . XMonad.StackSet.workspace . XMonad.StackSet.current $ winset

    let ws = myPprWindowSet sort' urgents pp winset

    wt <- maybe (return "") (fmap show . getName) . XMonad.StackSet.peek $ winset

    extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

    return $ encodeString . mySepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        , ppTitle  pp $ ppTitleSanitize pp wt
                        ]
                        ++ catMaybes extras

myPprWindowSet :: WorkspaceSort -> [Window] -> PP -> WindowSet -> String
myPprWindowSet sort' urgents pp s = mySepBy (ppWsSep pp) . map fmt . sort' $
            map XMonad.StackSet.workspace (XMonad.StackSet.current s : XMonad.StackSet.visible s) ++ XMonad.StackSet.hidden s
   where this            = XMonad.StackSet.currentTag s
         visibles        = map (XMonad.StackSet.tag . XMonad.StackSet.workspace) (XMonad.StackSet.visible s)
         screenShow      = (\w -> (\_ -> wrap (superScripsNumbers (show $ fromJust $ ((XMonad.StackSet.tag w) `elemIndex` visibles)) False) ""))

         fmt w = printer pp $ (windower w)
          where printer | any (\x -> maybe False (== XMonad.StackSet.tag w) (XMonad.StackSet.findTag x s)) urgents  = ppUrgent
                        | XMonad.StackSet.tag w == this                                                             = ppCurrent
                        | XMonad.StackSet.tag w `elem` visibles                                                     = screenShow w
                        | isJust (XMonad.StackSet.stack w)                                                          = ppHidden
                        | otherwise                                                                                 = ppHiddenNoWindows
                windower = (\tag -> XMonad.StackSet.tag tag ++ (superScripsNumbers (show $ (length . XMonad.StackSet.integrate' . XMonad.StackSet.stack) tag) True))

-- }}}

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

-- Border selection ambiguity function
data MyBorderAmbiguity = MyBorderAmbiguity deriving (Read, Show)
instance SetsAmbiguous MyBorderAmbiguity where
    hiddens _ wset lr mst wrs = floating
        where
            floating = [ w |
                        (w, XMonad.StackSet.RationalRect px py wx wy) <- Data.Map.toList . XMonad.StackSet.floating $ wset,
                        px <= 0, py <= 0,
                        wx + px >= 1, wy + py >= 1]

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

-- Replace with lists
replaceLists :: [Char] -> [Char] -> Char -> Char
replaceLists listSrc listDest subsNow
        | (isNothing fc) = subsNow
        | otherwise      = listDest !! (fromJust fc)
        where
                fc = elemIndex subsNow listSrc

-- List of superscripts and subscrips
superScripsNumbers :: String -> Bool -> String
superScripsNumbers numberString isSup =
        map (replaceLists listAgainst listNow) numberString
        where
                listNow
                        | (isSup == True) = "⁰¹²³⁴⁵⁶⁷⁸⁹"
                        | otherwise       = "₀₁₂₃₄₅₆₇₈₉"
                listAgainst = "0123456789"

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
                        myDynamicLogWithPP ppIn { ppOutput = hPutStrLn pipe }
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

    -- Set all needed environment variables
    mapM_ (\each -> setEnv (fst each) (snd each)) envVarsSet

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
                                  , argumentsToString $ myBarArguments
                                  , argumentsToString $ myBarColourArguments (addQuotes xrBarFore) (addQuotes xrBarBack)
                                  ]
    let myBarCommandBottom = unwords [ myBar
                                     , snd myBarConfigs
                                     , argumentsToString $ myBarArguments
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
            , layoutHook         = lessBorders MyBorderAmbiguity $
                                   spacingRawScalable xrSpace scaling $
                                   layoutHook def
            , manageHook         = myManageHook <+> manageHook def
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
