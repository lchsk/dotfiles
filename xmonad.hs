import System.IO
import System.Exit
import XMonad
import XMonad.Util.Cursor
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Layout.GridVariants
import XMonad.Config.Xfce
import XMonad.Actions.WindowBringer

myTerminal = "urxvt"
myFloatingTerminal = "~/dotfiles/scripts/show_term.sh"
myPythonTerminal = "~/dotfiles/scripts/show_python.sh"
myScreenLock = "~/dotfiles/scripts/lock_and_sleep.sh"
myScreenshot = "scrot 'screen_%Y_%m_%d_%T_$wx$h.png' -e 'mkdir -p ~/screenshots/; mv $f ~/screenshots/; gwenview ~/screenshots/$f'"
myRecompile = "killall conky dzen2 stalonetray && xmonad --recompile; xmonad --restart; notify-send 'xmonad recompiled!' -t 1000"
myRestart = "killall conky dzen2 stalonetray && xmonad --restart; notify-send 'xmonad restarted <3'"
myLauncher = "(which xstarter && urxvt -e xstarter) || urxvt -e ~/xstarter/bin/xstarter"

myExtraWorkspaces = [
  (xK_a, "a"),
  (xK_s, "s"),
  (xK_d, "d"),
  (xK_z, "z"),
  (xK_x, "x"),

  (xK_grave, "`"),
  (xK_1, "1"),
  (xK_2, "2"),
  (xK_3, "3"),
  (xK_4, "4"),
  (xK_5, "5"),
  (xK_6, "6"),
  (xK_7, "7"),
  (xK_8, "8"),
  (xK_9, "9"),
  (xK_0, "0"),
  (xK_minus, "-"),
  (xK_equal, "="),

  (xK_f, "f")
  ]

myWorkspaces = map snd myExtraWorkspaces

myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    noBorders (tabbed shrinkText tabConfig) |||
    ThreeColMid 0 (3/100) (1/3) |||
    Mirror (Tall 1 (3/100) (1/2))
    -- ||| Full
    ) |||
    noBorders (fullscreenFull Full)

color1 = "#eab700"
color2 = "#2ecc71"
black = "#000000"
white = "#bbbbbb"
grey = "#444444"

myNormalBorderColor = grey
myFocusedBorderColor = white

tabbedFont = "xft:Inconsolata:pixelsize=12"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
  fontName = tabbedFont,
    activeBorderColor = color1,
    activeTextColor = black,
    activeColor = color1,
    inactiveBorderColor = grey,
    inactiveTextColor = black,
    inactiveColor = grey
}

myManageHook = composeAll
  [
    resource  =? "desktop_window" --> doIgnore
  , className =? "knights"        --> doFloat
  , title     =? "xstarter"       --> doFloat
  , resource  =? "gpicview"       --> doFloat
  , resource  =? "viewnior"       --> doFloat
  , resource  =? "gwenview"       --> doFloat
  , className =? "mpv"            --> viewShift "f"
  , title     =? "SMPlayer"       --> viewShift "f"
  , title     =? "Okular"         --> viewShift "`"
  , title     =? "urxvtfloat"     --> doFloat
  , isFullscreen --> (doF W.focusDown <+> doFullFloat)
  ]
  where viewShift = doF . liftM2 (.) W.greedyView W.shift

-- Width of the window border in pixels.
myBorderWidth = 1

leftAlt = mod1Mask
rightAlt = mod3Mask
winKey = mod4Mask
myModMask = winKey

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Start a floating terminal. Size is defined in myFloatingTerminal variable.
  , ((modMask .|. shiftMask, xK_m),
     spawn myFloatingTerminal)

  -- Start a python shell term.
  , ((modMask .|. shiftMask, xK_n),
     spawn myPythonTerminal)

  -- Lock the screen
  , ((modMask .|. controlMask, xK_l),
     spawn myScreenLock)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn myLauncher)

  -- Take a full screenshot using the command specified by myScreenshot.
  , ((modMask .|. controlMask, xK_s),
     spawn myScreenshot)

  , ((modMask .|. controlMask, xK_i),
     spawn "(ps -ax | grep stalonetray | wc -l && pkill stalonetray) || exec stalonetray -i 19 -geometry 10x2+0 --icon-gravity SE -bg '#ff00ff'")

  -- Mute volume.
  , ((modMask .|. controlMask, xK_m),
     spawn "amixer -D pulse set Master toggle; notify-send `~/dotfiles/scripts/volume.sh` -t 1000")

  -- Decrease volume.
  , ((modMask .|. controlMask, xK_j),
     spawn "amixer -D pulse set Master 5%-; notify-send `~/dotfiles/scripts/volume.sh` -t 500")

  -- Increase volume.
  , ((modMask .|. controlMask, xK_k),
     spawn "amixer -D pulse set Master 5%+; notify-send `~/dotfiles/scripts/volume.sh` -t 500")

  -- Windows Bringer
  , ((modMask, xK_g), gotoMenu)
  , ((modMask, xK_b), bringMenu)

  -- (Spotify): Play/pause.
  , ((0, 0x1008ff14),
     spawn "~/dotfiles/thirdparty/sp play")

  -- (Spotify): Previous
  , ((0, 0x1008ff16),
     spawn "~/dotfiles/thirdparty/sp prev")

  -- (Spotify): Next
  , ((0, 0x1008ff17),
     spawn "~/dotfiles/thirdparty/sp next")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  ,  ((modMask .|. shiftMask, xK_equal), sendMessage $ IncMasterCols 1),
     ((modMask .|. shiftMask, xK_minus), sendMessage $ IncMasterCols (-1)),
     ((modMask .|. controlMask,  xK_equal), sendMessage $ IncMasterRows 1),
     ((modMask .|. controlMask,  xK_minus), sendMessage $ IncMasterRows (-1))

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     -- restart "xmonad" True)
     spawn myRecompile)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

  ++ [
        ((myModMask, key), (windows $ W.greedyView ws))
        | (key,ws) <- myExtraWorkspaces
      ] ++ [
        ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
        | (key,ws) <- myExtraWorkspaces
      ]


-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))
  ]

myStartupHook = do
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"

myLogHook h = dynamicLogWithPP $ defaultPP

    -- current workspace
    { ppCurrent = dzenColor black color1 . pad

    -- workspaces which contain windows
    , ppHidden  = dzenColor white black . pad

    , ppLayout  = dzenColor color1 black . (\layout -> case layout of
      "Tall"            -> "[|]"
      "ThreeCol"        -> "[3]"
      "Mirror Tall"     -> "[-]"
      "Tabbed Simplest" -> "[T]"
      "Full"            -> "[F]"
      otherwise         -> layout
      )
    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip

    , ppVisible = dzenColor color1 black . pad

    -- shorten if it goes over 100 characters
    , ppTitle           = dzenColor white black . shorten 100

    -- no separator between workspaces
    , ppWsSep           = ""

    -- put a few spaces between each object
    , ppSep             = " "

    -- output to the handle we were given as an argument
    , ppOutput          = hPutStrLn h
    }

main = do
  -- Black background
  bgColor <- spawnPipe "xsetroot -solid rgb:00/00/00"

  -- Top:
  xmonadBar <- spawnPipe "`~/dotfiles/scripts/dzen2.sh` -fn 'Inconsolata-10' -x 0 -y 0 -ta 'l' -dock -bg '#000000' -fg '#ff00ff'"
  -- tray <- spawnPipe "stalonetray -i 19 -geometry 10x1+100 --icon-gravity SE -bg '#000000'"
  nmApplet <- spawnPipe "nm-applet"

  -- Bottom:
  conkyBar <- spawnPipe "conky -c ~/dotfiles/conky_one | `~/dotfiles/scripts/dzen2.sh` -y -1 -fn 'Inconsolata-10' -ta 'l' -dock -bg '#000000' -fg '#ffffff' -x 0"
  -- slowBar <- spawnPipe "conky -c ~/dotfiles/conky_slow | `~/dotfiles/scripts/dzen2.sh` -y -1 -fn 'Inconsolata-9' -ta 'l' -dock -bg '#000000' -fg '#ffffff' -x 0 -w 400"
  -- fastBar <- spawnPipe "conky -c ~/dotfiles/conky_fast | `~/dotfiles/scripts/dzen2.sh` -y -1 -fn 'Inconsolata-9' -ta 'l' -dock -bg '#000000' -fg '#ffffff' -x 400"

  xmonad $ defaults {
    logHook = myLogHook xmonadBar
      , manageHook = manageDocks <+> myManageHook
}

defaults = xfceConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
}
