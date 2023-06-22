-- IMPORTS
import qualified Data.Map as M
import Control.Monad as C
import Data.List
import Data.Monoid
import Data.Maybe (fromJust)
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.SpawnOn
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.Warp
import XMonad.Actions.WindowNavigation
import XMonad.Actions.WithAll
import XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as EWMHD
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Dwindle
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutHints
import XMonad.Layout.LimitWindows
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Spacing
import XMonad.ManageHook
import qualified XMonad.StackSet as W
--import qualified DBus as D
--import qualified DBus.Client as D
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- setup color variables
import Colors.Stylix

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = colorBg
myFocusedBorderColor = colorFocus

-- Default apps
myTerminal, myBrowser :: String
myTerminal = "$TERM"
myBrowser = "$BROWSER"
myEditor = "$EDITOR"
mySpawnEditor = "$SPAWNEDITOR"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 3

-- Modmask
myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces =
  [ "<fn=1>\xf15c¹</fn>", -- document icon for writing
    "<fn=1>\xeb01 ²</fn>", -- globe icon for browsing
    "<fn=1>\xf121³</fn>", -- dev icon for programming
    "<fn=1>\xf0cb9 ⁴</fn>", -- music file icon for composition
    "<fn=1>\xf1fc⁵</fn>", -- paint icon for art
    "<fn=1>\xf0bdc ⁶</fn>", -- video icon for recording/editing
    "<fn=1>\xf0d6⁷</fn>", -- money icon for finances
    "<fn=1>\xf19d⁸</fn>", -- cap icon for teaching
    "<fn=1>\xf11b⁹</fn>" -- gamepad icon for gaming
  ]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm findTerm manageTerm,
    NS "ranger" spawnRanger findRanger manageRanger,
    NS "octave" spawnOctave findOctave manageOctave,
    NS "btm" spawnBtm findBtm manageBtm,
    NS "geary" spawnGeary findGeary manageGeary,
    NS "helpmenu" spawnHelp findHelp manageHelp,
    NS "musikcube" spawnMusikcube findMusikcube manageMusikcube,
    NS "cal" spawnCal findCal manageCal,
    NS "pavucontrol" spawnPavucontrol findPavucontrol managePavucontrol,
    NS "discord" spawnDiscord findDiscord manageDiscord
  ]
  where
    spawnTerm = myTerminal ++ " --title scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnRanger = "kitty --title ranger-scratchpad -e ranger"
    findRanger = title =? "ranger-scratchpad"
    manageRanger = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnOctave = myTerminal ++ " --title octave-scratchpad -e octave"
    findOctave = title =? "octave-scratchpad"
    manageOctave = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w
    spawnBtm = myTerminal ++ " -o font.size=12 --title btm-scratchpad -e btm"
    findBtm = title =? "btm-scratchpad"
    manageBtm = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w
    spawnDiscord = "gtkcord4"
    findDiscord = className =? "gtkcord4"
    manageDiscord = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w
    spawnGeary = "geary"
    findGeary = className =? "Geary"
    manageGeary = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w
    spawnHelp = myTerminal ++ " --title xmonad_helpmenu -e w3m ~/.xmonad/helpmenu.txt"
    findHelp = title =? "xmonad_helpmenu"
    manageHelp = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnMusikcube = myTerminal ++ " -o font.size=14 --title musikcube-scratchpad -e musikcube"
    findMusikcube = title =? "musikcube-scratchpad"
    manageMusikcube = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnCal = "gnome-calendar"
    findCal = className =? "gnome-calendar"
    manageCal = customFloating $ W.RationalRect l t w h
      where
        h = 0.4
        w = 0.3
        t = 0.45 - h
        l = 1 - w
    spawnPavucontrol = "pavucontrol"
    findPavucontrol = className =? "Pavucontrol"
    managePavucontrol = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.3
        t = 0.9 - h
        l = 0.65 - w

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [
    -- insert keybinds with array values of ((keybind, action))

      -- launch a terminal
      ((modm, xK_Return), spawn $ XMonad.terminal conf),

      -- launch emacsclient
      ((modm, xK_a), spawn mySpawnEditor),

      -- launch browser
      ((modm, xK_s), spawn myBrowser),

      -- take screenshots
      ((0, xK_Print), spawn "flameshot gui"), -- snip screenshot and save
      ((controlMask, xK_Print), spawn "flameshot gui --clipboard"), -- snip screenshot to clipboard
      ((shiftMask, xK_Print), spawn "flameshot screen"), -- screen capture current monitor and save
      ((controlMask .|. shiftMask, xK_Print), spawn "flameshot screen -c"), -- screen capture current monitor to clipboard

      -- launch game manager in gaming workspace
      ((modm, xK_g), spawn "xdotool key Super+9 && gamehub"),

      -- control brightness from kbd
      ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set +15"),
      ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 15-"),

      -- control kbd brightness from kbd
      ((0, xF86XK_KbdBrightnessUp), spawn "brightnessctl --device='asus::kbd_backlight' set +1 & xset r rate 350 100"),
      ((0, xF86XK_KbdBrightnessDown), spawn "brightnessctl --device='asus::kbd_backlight' set 1- & xset r rate 350 100"),
      ((shiftMask, xF86XK_MonBrightnessUp), spawn "brightnessctl --device='asus::kbd_backlight' set +1 & xset r rate 350 100"),
      ((shiftMask, xF86XK_MonBrightnessDown), spawn "brightnessctl --device='asus::kbd_backlight' set 1- & xset r rate 350 100"),

      -- control volume from kbd
      ((0, xF86XK_AudioLowerVolume), spawn "pamixer -d 10"),
      ((0, xF86XK_AudioRaiseVolume), spawn "pamixer -i 10"),
      ((0, xF86XK_AudioMute), spawn "pamixer -t"),

      -- control music from kbd
      --((0, xF86XK_AudioPlay), spawn "cmus-remote -u"),
      --((0, xF86XK_AudioStop), spawn "cmus-remote -s"),
      --((0, xF86XK_AudioNext), spawn "cmus-remote -n && ~/.local/bin/cmus-current-song-notify.sh"),
      --((0, xF86XK_AudioPrev), spawn "cmus-remote -r && ~/.local/bin/cmus-current-song-notify.sh"),

      -- launch rofi
      ((modm, xK_semicolon), spawn ("rofi -show drun -show-icons")),
      ((modm, xK_p), spawn ("keepmenu")),
      ((modm, xK_i), spawn ("networkmanager_dmenu")),

      -- close focused window
      ((modm, xK_q), kill),
      -- close all windows on current workspace
      ((modm .|. shiftMask, xK_c), killAll),
      -- exit xmonad
      ((modm .|. shiftMask, xK_q), spawn "killall xmonad-x86_64-linux"),
      -- Lock with dm-tool
      ((modm, xK_Escape), spawn "dm-tool switch-to-greeter"),
      -- Lock with dm-tool and suspend
      ((modm .|. shiftMask, xK_s), spawn "dm-tool switch-to-greeter & systemctl suspend"),
      ((modm .|. shiftMask, xK_Escape), spawn "dm-tool switch-to-greeter & systemctl suspend"),

      -- Rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
      --  Reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),

      -- Resize viewed windows to the correct size
      ((modm, xK_r), C.sequence_ [spawn "killall xmobar; autorandr -c; xmonad --restart;", refresh]),

      -- Move focus to window below
      ((modm, xK_j), C.sequence_ [windowGo D True, switchLayer, warpToWindow 0.5 0.5]),
      -- Move focus to window above
      ((modm, xK_k), C.sequence_ [windowGo U True, switchLayer, warpToWindow 0.5 0.5]),
      -- Move focus to window left
      ((modm, xK_h), C.sequence_ [windowGo L True, switchLayer, warpToWindow 0.5 0.5]),
      -- Move focus to window right
      ((modm, xK_l), C.sequence_ [windowGo R True, switchLayer, warpToWindow 0.5 0.5]),

      -- Move focus to screen below
      ((modm, xK_Down), C.sequence_ [screenGo D True, warpToCurrentScreen 0.5 0.5]),
      -- Move focus to screen up
      ((modm, xK_Up), C.sequence_ [screenGo U True, warpToCurrentScreen 0.5 0.5]),
      -- Move focus to screen left
      ((modm, xK_Left), C.sequence_ [screenGo L True, warpToCurrentScreen 0.5 0.5]),
      -- Move focus to screen right
      ((modm, xK_Right), C.sequence_ [screenGo R True, warpToCurrentScreen 0.5 0.5]),

      -- Swap with window below
      ((modm .|. shiftMask, xK_j), C.sequence_ [windowSwap D True, windowGo U True, switchLayer]),
      -- Swap with window above
      ((modm .|. shiftMask, xK_k), C.sequence_ [windowSwap U True, windowGo D True, switchLayer]),
      -- Swap with window left
      ((modm .|. shiftMask, xK_h), C.sequence_ [windowSwap L True, windowGo R True, switchLayer]),
      -- Swap with window right
      ((modm .|. shiftMask, xK_l), C.sequence_ [windowSwap R True, windowGo L True, switchLayer]),

      -- Shrink the master area
      ((modm .|. controlMask, xK_h), sendMessage Shrink),
      -- Expand the master area
      ((modm .|. controlMask, xK_l), sendMessage Expand),

      -- Swap the focused window and the master window
      ((modm, xK_m), windows W.swapMaster),

      -- Toggle tiling/floating status of window
      ((modm, xK_t), withFocused toggleFloat),

      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),

      -- scratchpad keybindings
      ((modm, xK_f), namedScratchpadAction myScratchPads "ranger"),
      --((modm, xK_x), namedScratchpadAction myScratchPads "keepassxc"),
      ((modm, xK_z), namedScratchpadAction myScratchPads "terminal"),
      ((modm, xK_b), namedScratchpadAction myScratchPads "btm"),
      ((modm, xK_d), namedScratchpadAction myScratchPads "discord"),
      ((modm, xK_o), namedScratchpadAction myScratchPads "octave"),
      ((modm, xK_e), namedScratchpadAction myScratchPads "geary"),
      ((modm, xK_n), namedScratchpadAction myScratchPads "musikcube"),
      ((modm, xK_c), namedScratchpadAction myScratchPads "cal"),
      ((modm, xK_y), namedScratchpadAction myScratchPads "pavucontrol"),
      ((modm, xK_slash), namedScratchpadAction myScratchPads "helpmenu")

      ]

      ++
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N

      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

  where
    -- toggle float/tiling status of current window
    toggleFloat w =
      windows
        ( \s ->
            if M.member w (W.floating s)
              then W.sink w s
              else (W.float w (W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)) s)
        )
    -- warp cursor to (x, y) coordinate of current screen
    warpToCurrentScreen x y = do
      sid <- withWindowSet $ return . W.screen . W.current
      warpToScreen sid x y
    -- TODO goto and warp (coords x, y) to window in DIRECTION, or goto and warp (coords x, y) to screen in DIRECTION if no window is available
    windowOrScreenGoAndWarp direction x y =
      do windowGo direction True

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    --    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm,  button1),
        ( \w ->
            focus w
              >> mouseMoveWindow w
              >> windows W.shiftMaster
        )
      ),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        ( \w ->
            focus w
              >> mouseResizeWindow w
              >> windows W.shiftMaster
        )
      )
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Layouts:

spcPx = 5

mySpacing = spacingRaw False (Border spcPx spcPx spcPx spcPx) True (Border spcPx spcPx spcPx spcPx) True

myLayout = fullscreenFocus $ draggingVisualizer $ avoidStruts $ layoutHintsToCenter $ (mySpacing $ (Full ||| mouseResizable ||| mouseResizableMirrored))
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall 1 (5 / 100) (1 / 2)

    dwindled = Dwindle R CW 1.1 1.1

    mouseResizable =
      mouseResizableTile
        { masterFrac = 0.51,
          slaveFrac = 0.51,
          draggerType = BordersDragger
        }

    mouseResizableMirrored =
      mouseResizableTile
        { masterFrac = 0.51,
          slaveFrac = 0.51,
          draggerType = BordersDragger,
          isMirrored = True
        }

-- Window rules:
myManageHook =
  composeAll
    [ title =? "Myuzi" --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9),
      title =? "octave-scratchpad" --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
      title =? "scratchpad" --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
      className =? "gtkcord4" --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
      title =? "ranger-scratchpad" --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9),
      title =? "btm-scratchpad" --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
      className =? "Geary" --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9),
      title =? "scratch_cfw" --> (customFloating $ W.RationalRect 0.58 0.04 0.42 0.7),
      title =? "xmonad_helpmenu" --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9),
      className =? "Pavucontrol" --> (customFloating $ W.RationalRect 0.05 0.04 0.5 0.35),
      className =? "Syncthing GTK" --> (customFloating $ W.RationalRect 0.53 0.50 0.46 0.45),
      className =? "Proton Mail Bridge" --> (customFloating $ W.RationalRect 0.59 0.66 0.40 0.30),
      className =? "Zenity" --> (customFloating $ W.RationalRect 0.45 0.4 0.1 0.2),
      resource =? "desktop_window" --> doIgnore,
      -- this gimp snippet is from Kathryn Anderson (https://xmonad.haskell.narkive.com/bV34Aiw3/layout-for-gimp-how-to)
      (className =? "Gimp" <&&> fmap ("color-selector" `isSuffixOf`) role) --> doFloat,
      (className =? "Gimp" <&&> fmap ("layer-new" `isSuffixOf`) role) --> doFloat,
      (className =? "Gimp" <&&> fmap ("-dialog" `isSuffixOf`) role) --> doFloat,
      (className =? "Gimp" <&&> fmap ("-tool" `isSuffixOf`) role) --> doFloat,
      -- end snippet
      resource =? "kdesktop" --> doIgnore,
      manageDocks
    ]
   where role = stringProperty "WM_WINDOW_ROLE"

-- Apply fullscreen manage and event hooks
myFullscreenManageHook = fullscreenManageHook
myFullscreenEventHook = fullscreenEventHook

-- Server mode event hook
myEventHook = serverModeEventHook

-- navigation 2d config required for visual window movement
myNavigation2DConfig = def {layoutNavigation = [("Tall", hybridOf sideNavigation $ hybridOf centerNavigation lineNavigation), ("Full", hybridOf sideNavigation centerNavigation)]
                          , floatNavigation = hybridOf lineNavigation centerNavigation
                          , screenNavigation = hybridOf lineNavigation centerNavigation}

--myPP = def { ppCurrent = xmobarColor colorFocus "" }
myPP = xmobarPP { ppTitle = xmobarColor colorFocus "",
                  ppCurrent = xmobarStripTags ["NSP"] . xmobarColor colorFocus "",
                  ppVisible = xmobarStripTags ["NSP"] . xmobarColor colorSecondary "",
                  ppHidden = xmobarStripTags ["NSP"] . xmobarColor colorFg "",
                  ppHiddenNoWindows = xmobarStripTags ["NSP"] . xmobarColor color01 "",
                  ppOrder = \(ws : _) -> [ws],
                  ppSep = " "
                }
mySB = statusBarProp "xmobar" (pure myPP)

-- Startup hook
myStartupHook = do
  spawnOnce ("~/.config/xmonad/startup.sh '" ++ colorBg ++ "' '" ++ colorFg ++ "' '" ++ colorFocus ++ "' '" ++ colorSecondary ++ "'")

-- Now run xmonad with all the defaults we set up.
main = do
  spawn ("xmobar -x 0")
  spawn ("xmobar -x 1")
  spawn ("xmobar -x 2")
  xmonad . withSB mySB $
    withNavigation2DConfig myNavigation2DConfig $
      fullscreenSupportBorder $
        docks $
         EWMHD.ewmh
          def
            { -- simple stuff
              terminal = myTerminal,
              focusFollowsMouse = myFocusFollowsMouse,
              clickJustFocuses = myClickJustFocuses,
              borderWidth = myBorderWidth,
              modMask = myModMask,
              workspaces = myWorkspaces,
              normalBorderColor = myNormalBorderColor,
              focusedBorderColor = myFocusedBorderColor,
              -- key bindings
              keys = myKeys,
              mouseBindings = myMouseBindings,
              -- hooks, layouts
              layoutHook = myLayout,
              manageHook = myManageHook <+> myFullscreenManageHook <+> namedScratchpadManageHook myScratchPads,
              handleEventHook = myEventHook <+> myFullscreenEventHook <+> fadeWindowsEventHook,
              logHook = (refocusLastLogHook >> nsHideOnFocusLoss myScratchPads),
              startupHook = myStartupHook
            }
