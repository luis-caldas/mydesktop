module Mine.Defaults where

-- {{{ Imports

import XMonad

import XMonad.Hooks.DynamicLog

import qualified XMonad.StackSet as StackSet

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

myBarConfigs = ( myBarConfigFolder ++ "/main.xmobarrc"
               , myBarConfigFolder ++ "/side.xmobarrc"
               ) where
                    myBarConfigFolder = "\"${HOME}\"/.config/xmobar"

myBarPP = def { ppCurrent         = wrap "[" "]"
              , ppSep             = " ||| "
              , ppHidden          = wrap "|" "|"
              , ppHiddenNoWindows = wrap " " " "
              , ppLayout          = const ""
              , ppTitle           = shorten 60
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
                  ("M-<Return>", spawn myTerminal)
                , ("M-n"       , spawn myBrowser)
                , ("M-<Space>" , spawn myLauncher)
                -- Killer
                , ("M-<Backspace>", kill)
                -- Navigation
                , ("M-["  , windows StackSet.focusUp)
                , ("M-/"  , windows StackSet.focusDown)
                , ("M-p"  , windows StackSet.swapMaster)
                , ("M-S-[", windows StackSet.swapUp)
                , ("M-S-/", windows StackSet.swapDown)
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
                ] ++
                -- Displays shortcut
                [ (("M" ++ shift ++ key), screenWorkspace sc >>= flip whenJust (windows . f))
                    | (key, sc) <- zip (map ("-"++) ["q", "w", "e", "r"]) [0..]
                    , (f, shift) <- [ (StackSet.view, "")
                                    , (\f -> StackSet.view f . StackSet.shift f, "-S")
                                    ]] ++
                -- Workspaces shortcuts
                [ (("M" ++ shift ++ key), windows $ f i)
                    | (i, key) <- zip myWorkspaces (map ("-"++) (map show [1..9]))
                    , (f, shift) <- [ (StackSet.greedyView, "")
                                    , (\i -> StackSet.greedyView i . StackSet.shift i, "-S")
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
