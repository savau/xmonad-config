import XMonad

import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.ThreeColumns (ThreeCol(..))

import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Window (WindowPrompt(..), windowPrompt, allWindows)

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

import qualified XMonad.StackSet as S

import Control.Monad (forM)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Exit
import System.IO


myXMonadDir = "~/.xmonad/"


main = do
  spawn "autorandr --change"
  nScreens <- countScreens
  xmprocs  <- sequence $ (\n -> spawnPipe $ "xmobar " <> myXMobarConfig n <> " --screen " <> show n) <$> [0..pred nScreens]
  xmonad $ docks def
    { modMask            = myModMask
    --, focusFollowsMouse  = False
    , focusedBorderColor = myMainColor
    , normalBorderColor  = "#000000"
    , workspaces         = Map.elems myWorkspaces

    -- key bindings
    , keys = myKeys

    -- Hooks, layouts
    , layoutHook  = avoidStruts myLayouts
    , logHook     = dynamicLogWithPP myPP
      { ppOutput  = \str -> mapM_ (\xmproc -> hPutStrLn xmproc str) xmprocs
      }
    , manageHook  = manageHook def <+> manageDocks <+> manageSpawn
    , startupHook = spawn (mySystemTray <> " --config " <> mySysTrayConf (pred nScreens)) >> mapM_ startApplication myStartupApplications >> setWMName "LG3D"
    }
    where
      startApplication (app, opts, mWorkspace) = maybe spawn spawnOn mWorkspace $ intercalate " " $ app : opts

myKeys conf = Map.fromList $
  [ ((myModMask, xK_Return), spawn $ XMonad.terminal conf)
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
  , ((myModMask .|. shiftMask, xK_Right), sendMessage NextLayout)

  , ((myModMask .|. controlMask, xK_space ), setLayout $ layoutHook conf)
  , ((myModMask .|. controlMask, xK_Left  ), sendMessage Shrink)
  , ((myModMask .|. controlMask, xK_Right ), sendMessage Expand)
  , ((myModMask .|. controlMask, xK_Up    ), sendMessage MirrorShrink)
  , ((myModMask .|. controlMask, xK_Down  ), sendMessage MirrorExpand)
  , ((myModMask .|. controlMask, xK_r     ), spawn $ "xmonad --recompile; " <> myXMonadRestart)

  , ((myModMask .|. altMask, xK_Up   ), spawn "~/.utils/backlight/backlight.sh 1")
  , ((myModMask .|. altMask, xK_Down ), spawn "~/.utils/backlight/backlight.sh 0")
  , ((myModMask .|. altMask, xK_space), xmonadPromptC myScreenLayouts' myXPromptConf{ defaultPrompter = const "Screen layout: " })
  ] ++
  ((\key -> ((myModMask, key), spawn "xscreensaver-command -lock; xset dpms force off")) <$> myLockScreenKeys') ++
  ((\key -> ((myModMask, key), xmonadPromptC (Map.toList mySysPromptOpts) myXPromptConf{ defaultPrompter = const "System: ", autoComplete = Just 0 })) <$> Set.toList mySystemKeys) ++
  ((\(key,app) -> ((myModMask .|. myFUAMask, key), spawn app)) <$> myFUAs') ++
  [ ((myModMask, key), windows $ S.greedyView ws)
    | (key,ws) <- myWorkspaces'
  ] ++
  [ ((myModMask .|. shiftMask, key), windows $ S.shift ws)
    | (key,ws) <- myWorkspaces'
  ]
  where
    myFUAs' = Map.toList myFUAs
    myLockScreenKeys' = Set.toList myLockScreenKeys
    myScreenLayouts' = Map.toList myScreenLayouts
    myWorkspaces' = Map.toList myWorkspaces

myScreenLayouts :: Map String (X ())
myScreenLayouts = Map.fromList $ (\sl -> (sl, spawn $ "~/.screenlayout/" <> sl <> ".sh; " <> myXMonadRestart)) <$> ["main", "home", "work"]

myLockScreenKeys :: Set KeySym
myLockScreenKeys = Set.fromList
  [ xK_minus
  , xK_ssharp
  ]

myStartupApplications :: [(String, [String], Maybe WorkspaceId)]
myStartupApplications = 
  [ ("xscreensaver"                , ["-no-splash"] , mempty    )
  , ("volumeicon"                  , mempty         , mempty    )
  , ("nm-applet"                   , mempty         , mempty    )
  , ("blueman-applet"              , mempty         , mempty    )
  , ("pamac-tray"                  , mempty         , mempty    )
  , ("keepassxc"                   , mempty         , mempty    )
  , ("megasync"                    , mempty         , mempty    )
  , ("LC_TIME=root.UTF-8 birdtray" , mempty         , mempty    )
--, ("thunderbird"                 , mempty         , Just "10" )
--, ("zulip"                       , mempty         , Just "9"  )
--, ("signal-desktop-beta"         , mempty         , Just "8"  )
  ]

-- frequently used applications
myFUAs :: Map KeySym String
myFUAs = Map.fromList
  [ (xK_f, "thunar"                         )  -- file manager
  , (xK_k, "keepassxc"                      )  -- password manager
  , (xK_w, "firefox"                        )  -- web browser
  , (xK_m, "LC_TIME=root.UTF-8 thunderbird" )  -- mail client
  , (xK_z, "zulip"                          )  -- chat client
  , (xK_c, "google-calendar-dark"           )  -- calendar client
  , (xK_s, "signal-desktop-beta"            )  -- messenger client
  , (xK_p, "pidgin"                         )  -- XMPP client
  , (xK_t, "texstudio"                      )  -- tex editor
  , (xK_o, "octave --gui"                   )  -- GNU Octave
  , (xK_j, "idea"                           )  -- IntelliJ IDEA
  ]

myWorkspaces :: Map KeySym String
myWorkspaces = Map.fromList $ zip ([xK_1..xK_9] ++ [xK_0]) $ show <$> [1..10]

myPP :: PP
myPP = xmobarPP
  { ppCurrent = xmobarColor myMainColor   mempty . wrap "[" "]"
  , ppUrgent  = xmobarColor myUrgentColor mempty . wrap "!" "!"
  , ppSep     = " | "
  }

-- auxiliary defs

myModMask, myFUAMask :: KeyMask
myModMask = mod4Mask   -- Mod == Super
myFUAMask = shiftMask  -- Mod+Shift+(a-z) for frequently used applications

myMainColor, myUrgentColor :: String
myMainColor   = "#0084ff"
myUrgentColor = "#ff0000"

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
myXMonadRestart = (concatMap (\(app,_,_) -> "killall " <> app <> "; ") myStartupApplications) <> "killall " <> mySystemTray <> "; killall xmobar; xmonad --restart"

myXMobarConfig :: Int -> String
myXMobarConfig n = myXMonadDir <> "xmobar-" <> show n <> ".hs"

mySystemTray :: String
mySystemTray = "stalonetray"
mySysTrayConf :: Int -> String
mySysTrayConf n = myXMonadDir <> "stalonetrayrc-" <> show n

mySystemKeys :: Set KeySym
mySystemKeys = Set.singleton xK_equal  -- TODO add dead_acute

mySysPromptOpts :: MonadIO m => Map String (m ())
mySysPromptOpts = Map.fromList
  [ ( "Logout"       , io $ exitWith ExitSuccess)
  , ( "Suspend"      , spawn "systemctl suspend")
  , ( "Hibernate"    , spawn "systemctl hibernate")
  , ( "Reboot"       , spawn "systemctl reboot")
  , ( "Poweroff"     , spawn "systemctl poweroff")
  ]

myXPromptConf :: XPConfig
myXPromptConf    = def
  { font         = "xft:Droid Sans Mono-10:antialias=true"
  , height       = 25
  , historySize  = 0
  }

-- convenience defs
altMask :: KeyMask
altMask = mod1Mask
