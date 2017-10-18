{-# LANGUAGE NamedFieldPuns #-}
import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.WindowGo
import XMonad.Actions.Volume1
import XMonad.Hooks.EwmhDesktops
import System.IO
import Control.Monad
import XMonad.Layout.NoBorders

main = xmonad =<< statusBar "xmobar" pp hideStat conf

pp = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
              
modbtn = mod4Mask

hideStat XConfig { modMask } = (modMask, xK_s)

conf = def { modMask = modbtn
           , terminal = "urxvt"
           , layoutHook = tiled ||| Mirror tiled ||| noBorders Full
           , handleEventHook = fullscreenEventHook
           , manageHook = isFullscreen --> doFullFloat
           } `additionalKeys`
           [ ((modbtn, xK_z), spawn "xscreensaver-command -lock")
           , ((modbtn, xK_x), spawn "xscreensaver-command -activate")
           , ((modbtn, xK_b), raiseMaybe (spawn "firefox -P default") (className =? "Firefox"))
           , ((modbtn, xK_e), raiseMaybe (spawn "emacsclient -c") (className =? "Emacs"))
           , ((modbtn, xK_F9), void toggleMute)
           , ((modbtn, xK_F10), void (lowerVolume 5))
           , ((modbtn, xK_F11), void (raiseVolume 5))
           ]
  where tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100
