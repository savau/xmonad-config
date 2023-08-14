{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Tuple.Curry

import DBus.Client

import System.Exit
import System.IO
import System.Log.DBus.Server
import System.Log.Logger

import XMonad

import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.ThreeColumns (ThreeCol(..))

import XMonad.Prompt
import XMonad.Prompt.Window (WindowPrompt(..), windowPrompt, allWindows)
import XMonad.Prompt.XMonad

import qualified XMonad.StackSet as StackSet

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (runProcessWithInput, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)


main = do
  nScreens <- countScreens
  xmonad . docks . ewmh $ def
    { modMask            = myModMask
    , focusedBorderColor = myMainColorDark
    , normalBorderColor  = "#000000"
    , workspaces         = (show . wsId) <$> Set.toList myWorkspaces

    -- Key bindings
    , keys = myKeys

    -- Hooks, layouts
    , layoutHook  = avoidStruts myLayouts
    , manageHook  = manageHook def <+> manageDocks <+> manageSpawn <+> myManageFloats
    , startupHook = setWMName "LG3D"
    -- , startupHook = mapM_ spawnApplication (Set.toList myStartupApplications) >> setWMName "LG3D"
    }

myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys conf = Map.fromList $
  [ 
    ((myModMask, xK_Return), spawn "xterm -e tmux")
  , ((myModMask, xK_q     ), kill)
  , ((myModMask, xK_Down  ), windows StackSet.focusDown)
  , ((myModMask, xK_Up    ), windows StackSet.focusUp)
  , ((myModMask, xK_comma ), sendMessage $ IncMasterN (-1))
  , ((myModMask, xK_period), sendMessage $ IncMasterN   1 )
  , ((myModMask, xK_d     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
  , ((myModMask, xK_g     ), windowPrompt myXPromptConf Goto  allWindows)
  , ((myModMask, xK_b     ), windowPrompt myXPromptConf Bring allWindows)
--, ((myModMask, xK_x     ), xmonadPrompt myXPromptConf)
  , ((myModMask, xK_space ), withFocused $ windows . StackSet.sink)

  , ((myModMask .|. shiftMask, xK_Down ), windows StackSet.swapDown)
  , ((myModMask .|. shiftMask, xK_Up   ), windows StackSet.swapUp)
  , ((myModMask .|. shiftMask, xK_Left ), sendMessage FirstLayout)
  , ((myModMask .|. shiftMask, xK_Right), sendMessage NextLayout)       -- cycle through layouts
  , ((myModMask .|. shiftMask, xK_space), setLayout $ layoutHook conf)  -- reset layout

  , ((myModMask .|. controlMask, xK_Left  ), sendMessage Shrink)
  , ((myModMask .|. controlMask, xK_Right ), sendMessage Expand)
  , ((myModMask .|. controlMask, xK_r     ), spawn $ "xmonad --recompile && " <> myXMonadRestart)
  , ((myModMask .|. controlMask, xK_k     ), spawn "xmodmap ~/.Xmodmap")

  , ((myModMask .|. altMask, xK_space), xmonadPromptC myScreenLayouts' myXPromptConf{ defaultPrompter = const "Screen layout: " })
  ] ++
  ((\key -> ((myModMask .|. controlMask, key), spawn "xscreensaver-command -lock")) <$> myLockScreenKeys') ++
  ((\key -> ((myModMask, key), myXMonadSysPrompt)) <$> Set.toList mySystemKeys) ++
  ((\(key,app) -> ((myModMask .|. myFUAMask, key), spawnApplication app)) <$> myFUAs') ++
  [ ((myModMask, xK_u), myU2WPrompt conf) ] ++
  [ ((myModMask, wsKeySym), windows $ (StackSet.greedyView . show) wsId)
    | Workspace{..} <- myWorkspaces'
  ] ++
  [ ((myModMask .|. shiftMask, wsKeySym), windows $ (StackSet.shift . show) wsId)
    | Workspace{..} <- myWorkspaces'
  ]
  where
    myFUAs'           = Map.toList myFUAs
    myLockScreenKeys' = Set.toList myLockScreenKeys
    myScreenLayouts'  = Map.toList myScreenLayouts
    myWorkspaces'     = Set.toList myWorkspaces

myManageFloats :: ManageHook
myManageFloats = composeAll
  [ className =? "MEGAsync" --> doFloat
  ]

myScreenLayouts :: Map String (X ())
myScreenLayouts = Map.fromList $ (\sl -> (sl, spawn $ "~/.screenlayout/" <> sl <> ".sh; " <> myXMonadRestart)) <$> ["main", "home", "work"]

myU2WPrompt :: XConfig Layout -> X ()
myU2WPrompt conf = xmonadPromptC (Map.toList myU2WPromptOpts) myU2WPromptConf where
  myU2WPromptConf :: XPConfig
  myU2WPromptConf = myXPromptConf
    { defaultPrompter = const "UniWorX > "
    , autoComplete    = Just 0
    }
  myU2WPromptOpts :: Map String (X ())
  myU2WPromptOpts = Map.fromList
    [ ( "u[2w]: develop@srv01.uniworx.de:~/u2w" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "launch-terminal/dev.sh --develop u2w\"" )
    , ( "f[radrive]: develop@srv01.uniworx.de:~/fradrive" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "launch-terminal/dev.sh --develop fradrive\"" )
--  , ( "z[sh]: zsh@srv01.uniworx.de" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "launch-terminal/dev.sh --project u2w\"" )
--  , ( "n[ix-shell]: nix-shell@srv01.uniworx.de" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "launch-terminal/dev.sh --nix-shell u2w\"" )
--  , ( "m[onitor]: monitor servers", spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "monitor/all_servers.sh\"" )
--  , ( "l[ocal]: shell@localhost:~/u2w" , spawn $ XMonad.terminal conf <> " -e \"source " <> myU2WUtilsDir <> "launch-terminal/local.sh\"" )
--  , ( "s[shfs-]m[ount]: mount SSHFS" , spawn $ myU2WUtilsDir <> "sshfs/start.sh" )
--  , ( "s[shfs-]u[nmount]: unmount SSHFS" , spawn $ myU2WUtilsDir <> "sshfs/stop.sh" )
    ]
  myU2WUtilsDir = "~/.utils/u2w/"

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
myXMonadRestart = "xmonad --restart"
-- myXMonadRestart = (concatMap (\Application{..} -> "pkill " <> appName <> "; ") $ Set.toList myStartupApplications) <> "xmonad --restart"

myLockScreenKeys :: Set KeySym
myLockScreenKeys = Set.fromList
  [ xK_minus   -- us layout
  , xK_ssharp  -- german layout
  ]

mySystemKeys :: Set KeySym
mySystemKeys = Set.fromList
  [ xK_equal  -- us layout
  , 65105     -- german layout; dead acute
  ]

myCheckNetwork :: String
myCheckNetwork = "pingcount=10; while [ $pingcount -gt 0 ]; do ping -n -c 1 1.1.1.1; rc=$?; if [[ $rc -eq 0 ]]; then ((pingcount = 0)); fi; ((pingcount = pingcount - 1)); sleep 0.5; done; "

myXMonadSysPrompt :: X ()
myXMonadSysPrompt = myXMonadSysPromptXfce where
  myXMonadSysPromptXfce = spawn "xfce4-session-logout"
  myXMonadSysPromptXmonad = xmonadPromptC (Map.toList mySysPromptOpts) myXPromptConf{ defaultPrompter = const "System: ", autoComplete = Just 0 } where
    mySysPromptOpts :: MonadIO m => Map String (m ())
    mySysPromptOpts = Map.fromList
      [ ( "Logout"       , io $ exitWith ExitSuccess)  -- FIXME: does not work with xfce4
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
  , position     = Top
  , borderColor  = myMainColorDark
  , bgColor      = myMainColorDark
  , alwaysHighlight = True
  , bgHLight = myMainColorDark
  , fgHLight = myUrgentColor
  }


data Application = Application
                   { appName        :: String
                   , appEnvironment :: Map String String
                   , appOptions     :: [String]
                   , appWorkspace   :: Maybe WorkspaceId
                   }
                   deriving (Eq, Ord, Show, Read)

-- | Spawns a given application with the given environment and options, and if given on a specific workspace
spawnApplication :: Application -> X ()
spawnApplication Application{..} = maybe spawn spawnOn appWorkspace $ intercalate " " $ (fmap (\(k,v) -> k<>"="<>v<>" ") $ Map.toList appEnvironment) <> (appName : appOptions)


-- TODO: Make myStartupApplications redundant by moving autostart to nix-config
-- | Applications that are automatically launched after starting XMonad
-- myStartupApplications :: Set Application
-- myStartupApplications = (Set.fromList . fmap (uncurryN Application))
--   [
-- --  ( "xfce4-power-manager"
-- --  , mempty, mempty, mempty
-- --  )
-- --, ( "volumeicon"
-- --  , mempty, mempty, mempty
-- --  )
--     ( "nm-applet"
--     , mempty, mempty, mempty
--     )
--   , ( "blueman-applet"
--     , mempty, mempty, mempty
--     )
-- --, ( "pamac-tray"  -- TODO: launch this iff on Arch Linux
-- --  , mempty, mempty, mempty
-- --  )
-- --, ( "keepassxc"
-- --  , mempty, mempty, mempty
-- --  )
-- --, ( "megasync"
-- --  , Map.fromList [ ("QT_SCALE_FACTOR","1") ], mempty, mempty  -- setting QT_SCALE_FACTOR=1 as a workaround to avoid immediate segfault, see https://github.com/meganz/MEGAsync/issues/443
-- --  )
-- --, ( "birdtray"
-- --  , Map.fromList [ ("LC_TIME","root.UTF-8") ], mempty, mempty
-- --  )
-- --, ( "zulip"
-- --  , mempty, mempty, mempty
-- --  )
-- --, ( "signal-desktop-beta"
-- --  , mempty, mempty, Just "8"
-- --  )
--   ]

-- | Frequently used applications that can be launched via Mod+Shift+<key>
myFUAs :: Map KeySym Application
myFUAs = Map.fromList
  [
    -- Basic applications
    ( xK_f  -- [F]ile manager
    , Application "thunar"
      mempty mempty mempty
    )
  , ( xK_k  -- [K]eepassxc password manager
    , Application "keepassxc"
      mempty mempty mempty
    )

  -- Web applications
  , ( xK_w  -- [W]eb browser
    , Application "firefox"
      mempty mempty mempty
    )
  -- , ( xK_c  -- [C]hromium
  --   , Application "chromium"
  --     mempty mempty mempty
  --   )
  , ( xK_m  -- -[M]ail client
    , Application "thunderbird"
      (Map.singleton "LC_TIME" "root.UTF-8") mempty mempty
    )

  -- Chat applications
  --, ( xK_e  -- [E]lement (matrix)
  --  , Application "firefox"
  --    mempty ["-P matrix", "-kiosk"] mempty
  --  )
  --, ( xK_z  -- [Z]ulip chat client
  --  , Application "zulip"
  --    mempty mempty mempty
  --  )
  --, ( xK_s  -- [S]ignal messenger client
  --  , Application "signal-desktop"
  --    mempty ["--use-tray-icon"] mempty
  --  )
  --, ( xK_p  -- [P]idgin XMPP client
  --  , Application "pidgin"
  --    mempty mempty mempty
  --  )

  -- IDEs
  , ( xK_t  -- [T]eX IDE  -- TODO: Make obsolete (move to neovim; ref: https://jdhao.github.io/2019/03/26/nvim_latex_write_preview/)
    , Application "texstudio"
      mempty mempty mempty
    )
  --, ( xK_i  -- IntelliJ [I]DEA
  --  , Application "idea"
  --    mempty mempty mempty
  --  )
  --, ( xK_o  -- GNU [O]ctave
  --  , Application "octave"
  --    mempty ["--gui"] mempty
  --  )

  -- Development and system administration applications
  --, ( xK_g  -- [G]rafana
  --  , Application "firefox"
  --    mempty ["-P grafana", "-kiosk"] mempty
  --  )
  ]


-- | Convenience definition for Mod1 (== Alt) key mask
altMask :: KeyMask
altMask = mod1Mask

-- | Convenience definition for the "no-key" mask (i.e. no key pressed)
noMask :: KeyMask
noMask = 0