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
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Layout.GridVariants

myTerminal = "urxvt"
myFloatingTerminal = "urxvt -name urxvtfloat --geometry 115x30"
myScreensaver = "i3lock -u -t"
myScreenshot = "scrot"
myRecompile = "killall conky dzen2 && xmonad --recompile; xmonad --restart; notify-send 'xmonad recompiled <3'"
myRestart = "killall conky dzen2 && xmonad --restart; notify-send 'xmonad restarted <3'"

myExtraWorkspaces = [
  (xK_0, "0"),
  (xK_minus, "-"),
  (xK_equal, "="),
  (xK_a, "a"),
  (xK_s, "s"),
  (xK_d, "d"),
  (xK_j, "j"),
  -- (xK_k, "k"),
  (xK_l, "l")
  ]
myWorkspaces = map show [1..9]  ++ (map snd myExtraWorkspaces)

myLauncher = "xstarter || ~/xstarter/bin/xstarter"

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

-- myNormalBorderColor = "#7c7c7c"
myNormalBorderColor = grey
-- myFocusedBorderColor = "#e32c57"
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
    -- [ className =? "Chromium"       --> doShift "1"
    -- , className =? "Google-chrome"  --> doShift "1"
    resource  =? "desktop_window" --> doIgnore
    -- , className =? "Galculator"     --> doFloat
    -- , className =? "Steam"          --> doFloat
    -- , className =? "Gimp"           --> doFloat
    , className =? "knights"        --> doFloat
    , title     =? "xstarter"       --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , title     =? "urxvtfloat"     --> doFloat
    -- , className =? "VirtualBox"     --> doShift "4"
    -- , className =? "thunderbird"    --> doShift "5"
    , className =? "stalonetray"    --> doIgnore
    -- , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ]

-- Color of current window title in xmobar.
-- xmobarTitleColor = "#e32c57"

-- Color of current workspace in xmobar.
-- xmobarCurrentWorkspaceColor = "#eb4509"

-- Width of the window border in pixels.
myBorderWidth = 1

leftAlt = mod1Mask
rightAlt = mod3Mask
winKey = mod4Mask
myModMask = winKey

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  , ((modMask .|. shiftMask, xK_f),
     spawn myFloatingTerminal)

  -- Lock the screen using command specified by myScreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn myScreensaver)

  -- , ((modMask .|. controlMask, xK_o),
     -- spawn "xmobar ~/.xmonad/xmobar.hs -d")

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn myLauncher)

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  --, ((modMask .|. shiftMask, xK_p),
  --   spawn mySelectScreenshot)

  -- Take a full screenshot using the command specified by myScreenshot.
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn myScreenshot)

  -- Mute volume.
  , ((modMask .|. controlMask, xK_m),
     spawn "amixer -D pulse set Master toggle")

  -- Decrease volume.
  , ((modMask .|. controlMask, xK_j),
     spawn "amixer -D pulse set Master 5%-")

  -- Increase volume.
  , ((modMask .|. controlMask, xK_k),
     spawn "amixer -D pulse set Master 5%+")

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

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
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
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

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

-- myStartupHook = return ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"

myLogHook h = dynamicLogWithPP $ defaultPP

    -- current workspace
    { ppCurrent         = dzenColor black color1 . pad

    -- workspaces which contain windows
    , ppHidden          = dzenColor white black . pad

    -- other workspaces
    -- , ppHiddenNoWindows = dzenColor "#606060" "" . pad

    -- , ppLayout          = dzenColor "#ff00ff" "" . pad
    -- , ppLayout = wrap "^ca(1,xdotool key super+space)" "^ca()" . dzenColor "#ff00ff" "" .
              -- (\x -> case x of
                  -- "tile" -> "^i(~/dotfiles/xbm/tile.xbm)"
                  -- "mtile" -> "^i(~/dotfiles/xbm/tile.xbm) M"
                  -- "btile" -> "^i(~/dotfiles/xbm/tile.xbm) B"
                  -- "Full" -> "^i(/home//dotfiles/xbm/full.xbm)"
                  -- )
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
  -- xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs -d"
  leftBar <- spawnPipe "dzen2 || ~/bin/dzen2 -fn 'Inconsolata-11' -x 0 -y -1 -w 800 -ta 'l'"
  rightBar <- spawnPipe "conky -c ~/dotfiles/conky | `dzen2 || ~/bin/dzen2 -x 800 -y -1 -fn 'Inconsolata-11' -ta 'r'`"

  xmonad $ defaults {
      -- logHook = dynamicLogWithPP $ xmobarPP {
      -- logHook = dynamicLogWithPP $ defaultPP {
            -- ppOutput = hPutStrLn xmproc1
          -- , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          -- , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          -- , ppCurrent = xmobarColor "#ebac54" ""
          -- , ppCurrent = xmobarColor "#ff00ff" ""
          -- , ppWsSep = " "
          -- , ppSep = " | "
        -- }
    logHook = myLogHook leftBar
      , manageHook = manageDocks <+> myManageHook
      -- , startupHook = setWMName "LG3D"
  
}

defaults = defaultConfig {
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

