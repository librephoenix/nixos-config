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

-- CUSTOM COLORS
colorSchemeList, colorSchemePrettyList :: [String]
colorBgNormalList, colorFgNormalList :: [String]
color01NormalList, color01BrightList, color02NormalList, color02BrightList :: [String]
color03NormalList, color03BrightList, color04NormalList, color04BrightList :: [String]
color05NormalList, color05BrightList, color06NormalList, color06BrightList :: [String]
color07NormalList, color07BrightList, color08NormalList, color08BrightList :: [String]
colorFocusList, colorSecondaryList :: [String]
colorScheme, colorSchemePretty :: String
colorBgNormal, colorFgNormal :: [Char]
color01Normal, color01Bright, color02Normal, color02Bright :: String
color03Normal, color03Bright, color04Normal, color04Bright :: String
color05Normal, color05Bright, color06Normal, color06Bright :: String
color07Normal, color07Bright, color08Normal, color08Bright :: String
colorFocus, colorSecondary :: String
gtkTheme :: String
alacrittyTheme :: String
doomEmacsTheme :: String

gruvboxIndex, solarizedIndex, draculaIndex, tomorrowNightIndex, tokyoNightIndex, oceanicNextIndex, ubuntuIndex :: Int
gruvboxIndex = 0
solarizedIndex = 1
draculaIndex = 2
tomorrowNightIndex = 3
tokyoNightIndex = 4
oceanicNextIndex = 5
oldHopeIndex = 6
ubuntuIndex = 7

-- color scheme arrays
colorSchemeList = ["gruvbox", "solarized", "dracula", "tomorrow-night", "tokyo-night", "oceanic-next", "old-hope"]
colorSchemePrettyList = ["Gruvbox Dark", "Solarized Dark", "Dracula", "Tomorrow Night", "Tokyo Night", "Oceanic Next", "Old Hope"]
gtkThemeList = ["MyGruvbox", "", "OfficialDracula", "MyGraphite", "", "MyOceanicNext", "SweetDark"] -- names of corresponding gtk themes
alacrittyThemeList = ["gruvbox_dark", "solarized_dark", "dracula", "tomorrow_night", "tokyo_night", "oceanic_next", "old_hope"]
doomEmacsThemeList = ["doom-gruvbox", "doom-solarized-dark", "doom-dracula", "doom-tomorrow-night", "doom-tokyo-night", "doom-oceanic-next", "doom-old-hope"]
colorBgNormalList = ["#282828", "#002b36", "#282a36", "#1d1f21", "#1a1b26", "#1b2b34", "#1c1d21"] -- normal bg
colorBgBrightList = ["#3b3838", "#113b3f", "#36343f", "#3d3f41", "#2a2b36", "#2b3b41", "#3c3d41"] -- lighter bg
trayerBgNormalList = ["0x00282828", "0x00002b36", "0x00282a36", "0x1d1f21", "0x1a1b26", "0x1b2b34", "0x1c1d21"] -- trayer tint
colorFgNormalList = ["#ebdbb2", "#839496", "#f8f8f2", "#c5c8c6", "#a9b1d6", "#d8dee9", "#cbcdd2"] -- normal fg
color01NormalList = ["#343428", "#073642", "#000000", "#1d1f21", "#32344a", "#29414f", "#45474f"] -- black
color01BrightList = ["#928374", "#002b36", "#555555", "#666666", "#444b6a", "#405860", "#65676f"] -- bright black
color02NormalList = ["#cc241d", "#dc3ddf", "#ff5555", "#cc6666", "#f7768e", "#ec5f67", "#eb3d54"] -- red
color02BrightList = ["#fb4934", "#cb4b16", "#ff1010", "#ff3334", "#ff7a93", "#ff3130", "#eb3d54"] -- bright red
color03NormalList = ["#98971a", "#859900", "#50fa7b", "#b5bd68", "#9ece6a", "#99c794", "#78bd65"] -- green
color03BrightList = ["#b8bb26", "#586e75", "#02fe03", "#9ec400", "#b9f27c", "#66fa56", "#78bd65"] -- bright green
color04NormalList = ["#d79921", "#b58900", "#f1fa8c", "#e6c547", "#e0af68", "#fac863", "#e5cd52"] -- yellow
color04BrightList = ["#fabd2f", "#657b83", "#ffff02", "#f0c674", "#ff9e64", "#ffca4f", "#e5cd52"] -- bright yellow
color05NormalList = ["#458588", "#268bd2", "#bd93f9", "#81a2be", "#7aa2f7", "#6699cc", "#4fb4d8"] -- blue
color05BrightList = ["#83a598", "#839496", "#4d31fd", "#81a2be", "#7da6ff", "#4477ee", "#4fb4d8"] -- bright blue
color06NormalList = ["#b16286", "#d33682", "#ff79c6", "#b29fbb", "#ad8ee6", "#c594c5", "#ef7c2a"] -- magenta
color06BrightList = ["#d3869b", "#6c71c4", "#ff20d8", "#b77ee0", "#bb9af7", "#d864d8", "#ef7c2a"] -- bright magenta
color07NormalList = ["#689d6a", "#2aa198", "#8be9fd", "#70c0ba", "#449dab", "#5fb3b3", "#4fb4d8"] -- cyan
color07BrightList = ["#8ec07c", "#93a1a1", "#03feff", "#54ced6", "#0db9d7", "#30d2d0", "#4fb4d8"] -- bright cyan
color08NormalList = ["#a89984", "#eee8d5", "#bbbbbb", "#676b71", "#787c99", "#65737e", "#cbcdd2"] -- white
color08BrightList = ["#ebdbb2", "#fdf6e3", "#ffffff", "#787a7e", "#acb0d0", "#d8dee9", "#cbcdd2"] -- bright white
colorFocusList = ["#458588", "#859900", "#ff79c6", "#e6c547", "#ff9e64", "#c594c5", "#eb3d54"] -- focus and run launcher color
colorSecondaryList = ["#d79921", "#dc3ddf", "#bbbbbb", "#70c0ba", "#0db9d7", "#fac863", "#4fb4d8"] -- secondary color

-- choose a color scheme
myColorScheme = oldHopeIndex

-- setup color variables
colorScheme = colorSchemeList !! myColorScheme
colorSchemePretty = colorSchemePrettyList !! myColorScheme
gtkTheme = gtkThemeList !! myColorScheme
alacrittyTheme = alacrittyThemeList !! myColorScheme
doomEmacsTheme = doomEmacsThemeList !! myColorScheme
colorBgNormal = colorBgNormalList !! myColorScheme -- normal bg
colorBgBright = colorBgBrightList !! myColorScheme -- lighter bg
trayerBgNormal = trayerBgNormalList !! myColorScheme -- trayer tint
colorFgNormal = colorFgNormalList !! myColorScheme -- normal fg
color01Normal = color01NormalList !! myColorScheme -- black
color01Bright = color01BrightList !! myColorScheme -- bright black
color02Normal = color02NormalList !! myColorScheme -- red
color02Bright = color02BrightList !! myColorScheme -- bright red
color03Normal = color03NormalList !! myColorScheme -- green
color03Bright = color03BrightList !! myColorScheme -- bright green
color04Normal = color04NormalList !! myColorScheme -- yellow
color04Bright = color04BrightList !! myColorScheme -- bright yellow
color05Normal = color05NormalList !! myColorScheme -- blue
color05Bright = color05BrightList !! myColorScheme -- bright blue
color06Normal = color06NormalList !! myColorScheme -- magenta
color06Bright = color06BrightList !! myColorScheme -- bright magenta
color07Normal = color07NormalList !! myColorScheme -- cyan
color07Bright = color07BrightList !! myColorScheme -- bright cyan
color08Normal = color08NormalList !! myColorScheme -- white
color08Bright = color08BrightList !! myColorScheme -- bright white
colorFocus = colorFocusList !! myColorScheme -- focus and run launcher color
colorSecondary = colorSecondaryList !! myColorScheme

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = colorBgNormal
myFocusedBorderColor = colorFocus

-- Default apps
myTerminal, myBrowser :: String
myTerminal = "alacritty -o font.size=20"
myBrowser = "librewolf"

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
  [ "<fn=1>\xf15c</fn>¹", -- document icon for writing
    "<fn=1>\xfa9e</fn>²", -- globe icon for browsing
    "<fn=1>\xf121</fn>³", -- dev icon for programming
    "<fn=1>\xf001</fn>⁴", -- music file icon for composition
    "<fn=1>\xf1fc</fn>⁵", -- paint icon for art
    "<fn=1>\xead9</fn>⁶", -- video icon for recording/editing
    "<fn=1>\xf0d6</fn>⁷", -- money icon for finances
    "<fn=1>\xf19d</fn>⁸", -- cap icon for teaching
    "<fn=1>\xf11b</fn>⁹" -- gamepad icon for gaming
  ]
--myWorkspaces =
--  [ "doc", "www", "dev", "mus", "art", "vid", "fin", "edu", "game"]

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
    NS "cmus" spawnCmus findCmus manageCmus,
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
    --spawnRanger = myTerminal ++ " --title ranger-scratchpad -e ranger"
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
    spawnDiscord = "flatpak run com.discordapp.Discord"
    findDiscord = className =? "discord"
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
    spawnCmus = myTerminal ++ " -o font.size=28 --title cmus-scratchpad -e cmus && cmus-remote -R && cmus-remote -S"
    findCmus = title =? "cmus-scratchpad"
    manageCmus = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnCal = "alacritty -o font.size=18 --title cal-scratchpad -e calcurse"
    findCal = title =? "cal-scratchpad"
    manageCal = customFloating $ W.RationalRect l t w h
      where
        h = 0.6
        w = 0.6
        t = 0.65 - h
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
      ((modm, xK_a), spawn "emacsclient -c -a 'emacs'"),

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
      ((0, xF86XK_AudioPlay), spawn "cmus-remote -u"),
      ((0, xF86XK_AudioStop), spawn "cmus-remote -s"),
      ((0, xF86XK_AudioNext), spawn "cmus-remote -n && ~/.local/bin/cmus-current-song-notify.sh"),
      ((0, xF86XK_AudioPrev), spawn "cmus-remote -r && ~/.local/bin/cmus-current-song-notify.sh"),

      -- manage multiple monitors with kbd
      -- ((0, xF86XK_Explorer), spawn "/home/librephoenix/.local/bin/setup_external_monitor.sh"),
      -- ((0, xK_F8), spawn "/home/librephoenix/.local/bin/setup_external_monitor.sh"),

      -- launch dmenu
      --((modm, xK_semicolon), spawn ("dmenu_run -nb '" ++ colorBgNormal ++ "' -nf '" ++ color08Bright ++ "' -sb '" ++ colorFocus ++ "' -sf '" ++ color08Bright ++ "' -fn 'UbuntuMono-R:regular:pixelsize=28' -l 4 -p '➤'")),
      ((modm, xK_semicolon), spawn ("rofi -show drun -show-icons")),
      ((modm, xK_p), spawn ("keepmenu")),

      -- launch app template dmenu script
      ((modm, xK_w), spawn ("~/.xmonad/template-select.sh '" ++ colorBgNormal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ color08Bright ++ "'")),

      -- launch virt-manager vm select dmenu script
      ((modm, xK_v), spawn ("~/.xmonad/vm-select.sh '" ++ colorBgNormal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ color08Bright ++ "'")),
      -- launch virt-manager vm select dmenu script
      -- ((modm .|. shiftMask, xK_v), spawn ("~/.xmonad/vm-app-select.sh '" ++ colorBgNormal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ color08Bright ++ "'")),

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
      ((modm, xK_r), refresh),

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
      ((modm, xK_n), namedScratchpadAction myScratchPads "cmus"),
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
      className =? "discord" --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
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

-- Startup hook
myStartupHook = do
  spawnOnce ("~/.config/xmonad/startup.sh '" ++ trayerBgNormal ++ "' '" ++ colorBgNormal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ color08Bright ++ "' '" ++ gtkTheme ++ "' '" ++ alacrittyTheme ++ "' '" ++ doomEmacsTheme ++ "' '" ++ color01Normal ++ "' '" ++ color01Bright ++ "' '" ++ color02Normal ++ "' '" ++ color02Bright ++ "' '" ++ color03Normal ++ "' '" ++ color03Bright ++ "' '" ++ color04Normal ++ "' '" ++ color04Bright ++ "' '" ++ color05Normal ++ "' '" ++ color05Bright ++ "' '" ++ color06Normal ++ "' '" ++ color06Bright ++ "' '" ++ color07Normal ++ "' '" ++ color07Bright ++ "' '" ++ color08Normal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ colorSecondary ++ "' '" ++ colorBgBright ++ "'")

--myPP = def { ppCurrent = xmobarColor colorFocus "" }
myPP = xmobarPP { ppTitle = xmobarColor colorFocus "",
                  ppCurrent = xmobarStripTags ["NSP"] . xmobarColor colorFocus "",
                  ppVisible = xmobarStripTags ["NSP"] . xmobarColor colorSecondary "",
                  ppHidden = xmobarStripTags ["NSP"] . xmobarColor colorFgNormal "",
                  ppHiddenNoWindows = xmobarStripTags ["NSP"] . xmobarColor colorBgBright "",
                  ppOrder = \(ws : _) -> [ws],
                  ppSep = " "
                }
mySB = statusBarProp "xmobar" (pure myPP)

-- Now run xmonad with all the defaults we set up.
main = do
  spawn ("xmobar -x 0 /home/librephoenix/.config/xmobar/xmobarrc")
  spawn ("xmobar -x 1 /home/librephoenix/.config/xmobar/xmobarrc")
  spawn ("xmobar -x 2 /home/librephoenix/.config/xmobar/xmobarrc")
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
