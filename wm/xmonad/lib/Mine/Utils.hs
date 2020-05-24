{-# LANGUAGE FlexibleContexts #-}
module Mine.Utils where

-- {{{ Imports

import XMonad

import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run

import Data.Maybe

import Data.List

import qualified Data.Text
import qualified Data.Map

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
