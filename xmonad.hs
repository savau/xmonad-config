{-# LANGUAGE RecordWildCards #-}

import Control.Monad (ap, forM)

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import System.Exit
import System.IO

import XMonad

import XMonad.Actions.SpawnOn (manageSpawn)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.ThreeColumns (ThreeCol(..))

import XMonad.Prompt
import XMonad.Prompt.Window (WindowPrompt(..), windowPrompt, allWindows)
import XMonad.Prompt.XMonad

import qualified XMonad.StackSet as S

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

import Applications
import Utils.KeyMask


myXMonadDir = "~/.xmonad/"

myStatusBarDir  = myXMonadDir <> "status-bar/"
mySystemTrayDir = myXMonadDir <> "system-tray/"


main = do
  nScreens <- countScreens
  xmprocs  <- sequence $ (\n -> spawnPipe $ "pingcount=10; while [ $pingcount -gt 0 ]; do ping -n -c 1 1.1.1.1; rc=$?; if [[ $rc -eq 0 ]]; then ((pingcount = 0)); fi; ((pingcount = pingcount - 1)); sleep 0.5; done; " <> myStatusBar <> " " <> myXMobarConfig n <> " --screen " <> show n) <$> [0..pred nScreens]
  xmonad $ docks def
    { modMask            = myModMask
  --, focusFollowsMouse  = False
    , focusedBorderColor = myMainColorDark
    , normalBorderColor  = "#000000"
    , workspaces         = (show . wsId) <$> Set.toList myWorkspaces

    -- key bindings
    , keys = myKeys

    -- Hooks, layouts
    , layoutHook  = avoidStruts myLayouts
    , logHook     = dynamicLogWithPP myPP
      { ppOutput  = \str -> mapM_ (\xmproc -> hPutStrLn xmproc str) xmprocs
      }
    , manageHook  = manageHook def <+> manageDocks <+> manageSpawn
    , startupHook = spawn (mySystemTray <> " --config " <> mySysTrayConf (pred nScreens)) >> mapM_ spawnApplication (Set.toList myStartupApplications) >> setWMName "LG3D"
    }

myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys conf = Map.fromList $
  [ 
    ((myModMask, xK_Return), spawn $ XMonad.terminal conf)
  , ((myModMask, xK_q     ), kill)
  , ((myModMask, xK_Down  ), windows S.focusDown)
  , ((myModMask, xK_Up    ), windows S.focusUp)
  , ((myModMask, xK_comma ), sendMessage $ IncMasterN (-1))
  , ((myModMask, xK_period), sendMessage $ IncMasterN   1 )
  , ((myModMask, xK_d     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
  , ((myModMask, xK_g     ), windowPrompt myXPromptConf Goto allWindows)
  , ((myModMask, xK_b     ), windowPrompt myXPromptConf Bring allWindows)
--, ((myModMask, xK_x     ), xmonadPrompt myXPromptConf)
  , ((myModMask, xK_space ), withFocused $ windows . S.sink)

  , ((myModMask .|. shiftMask, xK_Down ), windows S.swapDown)
  , ((myModMask .|. shiftMask, xK_Up   ), windows S.swapUp)
  , ((myModMask .|. shiftMask, xK_Left ), sendMessage FirstLayout)
  , ((myModMask .|. shiftMask, xK_Right), sendMessage NextLayout)       -- cycle through layouts
  , ((myModMask .|. shiftMask, xK_space), setLayout $ layoutHook conf)  -- reset layout

  , ((myModMask .|. controlMask, xK_Left  ), sendMessage Shrink)
  , ((myModMask .|. controlMask, xK_Right ), sendMessage Expand)
  , ((myModMask .|. controlMask, xK_r     ), spawn $ "xmonad --recompile && " <> myXMonadRestart)
  , ((myModMask .|. controlMask, xK_k     ), spawn "xmodmap ~/.Xmodmap")

--, ((myModMask .|. altMask, xK_Up   ), spawn "~/.utils/backlight/backlight.sh 1")
--, ((myModMask .|. altMask, xK_Down ), spawn "~/.utils/backlight/backlight.sh 0")
  , ((myModMask .|. altMask, xK_space), xmonadPromptC myScreenLayouts' myXPromptConf{ defaultPrompter = const "Screen layout: " })
  ] ++
  ((\key -> ((myModMask .|. controlMask, key), spawn "xscreensaver-command -lock")) <$> myLockScreenKeys') ++
  [ ((myModMask, xK_s), spawn "xfce4-screenshooter") ] ++
  ((\key -> ((myModMask, key), myXMonadSysPrompt)) <$> Set.toList mySystemKeys) ++
  ((\(key,app) -> ((myModMask .|. myFUAMask, key), spawnApplication app)) <$> myFUAs') ++
  [ ((myModMask, xK_u), myU2WPrompt conf) ] ++
  [ ((myModMask, wsKeySym), windows $ (S.greedyView . show) wsId)
    | Workspace{..} <- myWorkspaces'
  ] ++
  [ ((myModMask .|. shiftMask, wsKeySym), windows $ (S.shift . show) wsId)
    | Workspace{..} <- myWorkspaces'
  ]
  where
    myFUAs'           = Map.toList myFUAs
    myLockScreenKeys' = Set.toList myLockScreenKeys
    myScreenLayouts'  = Map.toList myScreenLayouts
    myWorkspaces'     = Set.toList myWorkspaces

myScreenLayouts :: Map String (X ())
myScreenLayouts = Map.fromList $ (\sl -> (sl, spawn $ "~/.screenlayout/" <> sl <> ".sh; " <> myXMonadRestart)) <$> ["main", "home", "work"]

myLockScreenKeys :: Set KeySym
myLockScreenKeys = Set.fromList
  [ xK_minus
  , xK_ssharp
  ]

myU2WPrompt :: XConfig Layout -> X ()
myU2WPrompt conf = xmonadPromptC (Map.toList myU2WPromptOpts) myU2WPromptConf where
  myU2WPromptConf :: XPConfig
  myU2WPromptConf = myXPromptConf
    { defaultPrompter = const "[u2w] > "
    , autoComplete    = Just 0
    }
  myU2WPromptOpts :: Map String (X ())
  myU2WPromptOpts = Map.fromList
    [ ( "d[evelop] : develop-shell@uni2work-dev1" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "launch-terminal/uni2work-dev1.sh --shell nix-shell --command develop\"" )
    , ( "z[sh] : zsh@uni2work-dev1" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "launch-terminal/uni2work-dev1.sh --shell nix-shell --command zsh\"" )
    , ( "n[ix-shell] : nix-shell@uni2work-dev1" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "launch-terminal/uni2work-dev1.sh --shell nix-shell\"" )
    , ( "p[g_top] : pg_top@uniworxdb2" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "monitor/pg_top.sh\"" )
    , ( "l[ocal] : shell@localhost:~/u2w" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "launch-terminal/local.sh\"" )
    , ( "sm [sshfs-mount] : mount SSHFS" , spawn $ myU2WUtilsDir <> "sshfs/start.sh" )
    , ( "su [sshfs-unmount] : unmount SSHFS" , spawn $ myU2WUtilsDir <> "sshfs/stop.sh" )
    ]
  myU2WUtilsDir = "~/.util/u2w/"

-- Wrapper type to map workspaces 
data Workspace = Workspace
                 { wsId     :: Int
                 , wsKeySym :: KeySym
                 }
                 deriving (Eq, Show, Read)

instance Ord Workspace where
  compare (Workspace i _) (Workspace j _) = compare i j

myWorkspaces :: Set Workspace
myWorkspaces = Set.fromList $ (\(wsId,wsKeySym) -> Workspace{..}) <$> (zip (10:[1..9]) [xK_0..xK_9])

myPP :: PP
myPP = xmobarPP
  { ppCurrent = xmobarColor myFocusColor     mempty . wrap "[" "]"
  , ppHidden  = xmobarColor myMainColorLight mempty
  , ppUrgent  = xmobarColor myUrgentColor    mempty . wrap "!" "!"
  , ppSep     = " | "
  }


-- auxiliary defs

myModMask, myFUAMask :: KeyMask
myModMask = mod4Mask   -- Mod == Super
myFUAMask = shiftMask  -- Mod+Shift+(a-z) for frequently used applications

myMainColorLight, myMainColorDark, myFocusColor, myUrgentColor :: String
myMainColorLight = "#7c818c"
myMainColorDark  = "#383c4a"
myFocusColor     = "#ffffff"
myUrgentColor    = "#5294e2"

myLayouts = 
              Tall nMaster delta frac
          ||| Mirror (Tall nMaster delta frac)
          ||| ThreeCol nMaster delta frac
          ||| Mirror (ThreeCol nMaster delta frac)
       -- ||| ResizableTall nMaster delta frac [1]
          ||| spiral (6/7)
          ||| Full
          where
            nMaster = 1
            delta   = 3/100
            frac    = 1/2

myXMonadRestart :: String
myXMonadRestart = (concatMap (\Application{..} -> "pkill " <> appName <> "; ") $ Set.toList myStartupApplications) <> "pkill " <> mySystemTray <> "; pkill " <> myStatusBar <> "; xmonad --restart"

myStatusBar :: String
myStatusBar = "xmobar"
myXMobarConfig :: Int -> String
myXMobarConfig n = myStatusBarDir <> "xmobar/xmobar-" <> show n <> ".hs"

mySystemTray :: String
mySystemTray = "stalonetray"
mySysTrayConf :: Int -> String
mySysTrayConf n = mySystemTrayDir <> "stalonetray/stalonetrayrc-" <> show n

mySystemKeys :: Set KeySym
mySystemKeys = Set.singleton xK_equal

myXMonadSysPrompt :: X ()
myXMonadSysPrompt = spawn "xfce4-session-logout"
-- obsolete with xfce4-session-logout
--myXMonadSysPrompt = xmonadPromptC (Map.toList mySysPromptOpts) myXPromptConf{ defaultPrompter = const "System: ", autoComplete = Just 0 } where
--  mySysPromptOpts :: MonadIO m => Map String (m ())
--  mySysPromptOpts = Map.fromList
--    [ ( "Logout"       , io $ exitWith ExitSuccess)  -- FIXME: does not work with xfce4
--    , ( "Suspend"      , spawn "systemctl suspend")
--    , ( "Hibernate"    , spawn "systemctl hibernate")
--    , ( "Reboot"       , spawn "systemctl reboot")
--    , ( "Poweroff"     , spawn "systemctl poweroff")
--    ]

myXPromptConf :: XPConfig
myXPromptConf    = def
  { font         = "xft:Droid Sans Mono-10:antialias=true"
  , height       = 25
  , historySize  = 0
  , position     = Top
  , borderColor  = myMainColorDark
  , bgColor      = myMainColorDark
  , alwaysHighlight = True
  , bgHLight = myMainColorDark
  , fgHLight = myUrgentColor
  }
