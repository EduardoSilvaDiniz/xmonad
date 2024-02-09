import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing

import XMonad.Util.EZConfig
import XMonad.Util.Loggers

import Graphics.X11.ExtraTypes.XF86

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig


myConfig = def
    {
      manageHook = myManageHook  -- Match on certain windows
    , terminal = "st"
    , focusedBorderColor = "#d79921"
    , borderWidth = 2
    , layoutHook = gaps [(U,5), (R,5), (L,5), (D,5)] $ spacing 5 $ Tall 1 (6/100) (1/2)
    , startupHook = do spawn "~/.config/xmonad/scripts/startup.sh"
    }
  `additionalKeysP`
    [
    -- ("M-C-s", unGrab *> spawn "scrot -s"        	     )
      ("M-p"  , spawn "rofi -show drun"           	     )
    , ("M-f"  , spawn "gtk-launch org.gnome.Nautilus.desktop")
    , ("M-b"  , spawn "gtk-launch google-chrome.desktop"     )
    , ("M-m"  , spawn "gtk-launch emacsclient.desktop"       )
    ]
  `additionalKeys`
    [
      ((0, xF86XK_PowerDown),         spawn "sudo systemctl suspend")
    , ((0, xF86XK_AudioRaiseVolume),  spawn "amixer -D pipewire set Master 3%+")
    , ((0, xF86XK_AudioLowerVolume),  spawn "amixer -D pipewire set Master 3%-")
    , ((0, xF86XK_AudioMute),         spawn "amixer -D pipewire set Master toggle")
    , ((0, xF86XK_MonBrightnessUp),   spawn "brightnessctl set +2%")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 2%-")
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [
      className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    {
      ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#d79921" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . orange  . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 40

    orange, blue, lowWhite, magenta, red, white, yellow :: String -> String
    orange   = xmobarColor "#d79921" ""
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
