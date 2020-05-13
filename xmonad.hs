import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.ThreeColumns (ThreeCol(..))

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

import qualified XMonad.StackSet as S

import Control.Monad (forM)
import qualified Data.Map as Map (fromList)
import System.Exit
import System.IO


main = do
  nScreens <- countScreens
  xmprocs  <- sequence $ (\n -> spawnPipe $ "xmobar " <> myXMobarConfig n <> " --screen " <> show n) <$> [0..pred nScreens]
  xmonad $ docks def
    { modMask            = myModMask
    --, focusFollowsMouse  = False
    , focusedBorderColor = myMainColor
    , workspaces         = snd <$> myWorkspaces

    -- key bindings
    , keys = myKeys

    -- Hooks, layouts
    , layoutHook  = avoidStruts myLayouts
    , logHook     = dynamicLogWithPP myPP
      { ppOutput  = \str -> mapM_ (\xmproc -> hPutStrLn xmproc str) xmprocs
      }
    , manageHook  = manageHook def <+> manageDocks
    , startupHook = mapM_ spawn myStartupApplications
    }

myKeys conf = Map.fromList $
  [
  -- basics
    ((myModMask, xK_Return), spawn $ XMonad.terminal conf)
  , ((myModMask, xK_q     ), kill)
  , ((myModMask, xK_Down  ), windows S.focusDown)
  , ((myModMask, xK_Up    ), windows S.focusUp)
  , ((myModMask, xK_comma ), sendMessage $ IncMasterN (-1))
  , ((myModMask, xK_period), sendMessage $ IncMasterN   1 )
  , ((myModMask, xK_d     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

  , ((myModMask .|. shiftMask, xK_Down ), windows S.swapDown)
  , ((myModMask .|. shiftMask, xK_Up   ), windows S.swapUp)
  , ((myModMask .|. shiftMask, xK_space), sendMessage NextLayout)

  , ((myModMask .|. controlMask, xK_space ), setLayout $ layoutHook conf)
  , ((myModMask .|. controlMask, xK_Left  ), sendMessage Shrink)
  , ((myModMask .|. controlMask, xK_Right ), sendMessage Expand)
  , ((myModMask .|. controlMask, xK_Up    ), sendMessage MirrorShrink)
  , ((myModMask .|. controlMask, xK_Down  ), sendMessage MirrorExpand)
  , ((myModMask .|. controlMask, xK_r     ), spawn $ "xmonad --recompile; " <> myXMonadRestart)
  , ((myModMask .|. controlMask, xK_q     ), io $ exitWith ExitSuccess)

  , ((myModMask .|. altMask, xK_Up  ), spawn "~/.utils/backlight/backlight.sh 1")
  , ((myModMask .|. altMask, xK_Down), spawn "~/.utils/backlight/backlight.sh 0")
  ] ++
  ((\(key,layout) -> ((myModMask .|. altMask, key), spawn $ "~/.screenlayout/" <> layout <> ".sh; " <> myXMonadRestart)) <$> myScreenLayouts) ++
  ((\key -> ((myModMask, key), spawn "xscreensaver-command -lock; xset dpms force off")) <$> myLockScreenKeys) ++
  ((\(key,app) -> ((myModMask .|. myFUAMask, key), spawn app)) <$> myFUAs) ++
  [ ((myModMask, key), windows $ S.greedyView ws)
    | (key,ws) <- myWorkspaces
  ] ++
  [ ((myModMask .|. shiftMask, key), windows $ S.shift ws)
    | (key,ws) <- myWorkspaces
  ]

myScreenLayouts = 
  [ (xK_m, "main")
  , (xK_h, "home")
  , (xK_w, "work")
  ]

myLockScreenKeys = [xK_minus, xK_ssharp]

myStartupApplications = 
  [ "stalonetray"
  , "nm-applet"
  , "blueman-applet"
  , "volumeicon"
  , "pamac-tray"
  , "keepassxc"
  , "megasync"
  ]

-- frequently used applications
myFUAs =
  [ (xK_f, "pcmanfm"    )  -- file manager
  , (xK_k, "keepassxc"  )  -- password manager
  , (xK_w, "firefox"    )  -- web browser
  , (xK_m, "thunderbird")  -- mail client
  , (xK_c, "zulip"      )  -- chat client
  , (xK_t, "texstudio"  )  -- tex editor
  ]

myWorkspaces = zip ([xK_1..xK_9] ++ [xK_0]) $ show <$> [1..10]

myPP = xmobarPP
  { ppCurrent = xmobarColor myMainColor   mempty . wrap "[" "]"
  , ppUrgent  = xmobarColor myUrgentColor mempty . wrap "!" "!"
  , ppSep     = " | "
  }

-- auxiliary defs
myModMask       = mod4Mask   -- Mod == Super
myFUAMask       = shiftMask  -- Mod+Shift+(a-z) for frequently used applications
myMainColor     = "#0084ff"
myUrgentColor   = "#ff0000"
myLayouts       =   ThreeCol nMaster delta frac
                ||| Mirror (ThreeCol nMaster delta frac)
                ||| Tall nMaster delta frac
                ||| Mirror (Tall nMaster delta frac)
                -- ||| ResizableTall nMaster delta frac [1]
                ||| spiral (6/7)
                ||| Full
                where
                  nMaster = 1
                  delta   = 3/100
                  frac    = 1/2
myXMonadRestart = concatMap (\app -> "killall " <> app <> "; ") myStartupApplications <> "killall xmobar; xmonad --restart"
myXMobarConfig n = "~/.xmonad/xmobar-dual-" <> show n <> ".hs"

altMask = mod1Mask

toggleStrutsKey XConfig{XMonad.modMask = modMask} = (modMask .|. controlMask, xK_b)
