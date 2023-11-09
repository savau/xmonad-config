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
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.Grid (Grid(..))

import XMonad.Prompt
import XMonad.Prompt.Window (WindowPrompt(..), windowPrompt, allWindows)
import XMonad.Prompt.XMonad

import qualified XMonad.StackSet as StackSet

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (runProcessWithInput, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)


main = do
--nScreens <- countScreens
  xmonad . docks . ewmh $ def
    { terminal           = "xfce4-terminal"
    , modMask            = myModMask
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
myKeys conf = Map.fromList $ concat
  [ [ ((myModMask                , key), comm) | (key, comm) <- Map.toList myMainCommands   ]
  , [ ((myModMask .|. controlMask, key), comm) | (key, comm) <- Map.toList mySystemCommands ]
  , [ ((myModMask .|. shiftMask  , key), spawnApplication app) | (key, app) <- Map.toList myFUAs ]
  ]
  where
    myMainCommands :: Map KeySym (X ())
    myMainCommands = Map.fromList $
      [ (xK_Return , spawn $ XMonad.terminal conf)
      , (xK_q      , kill)
      , (xK_k      , windows StackSet.focusUp)
      , (xK_j      , windows StackSet.focusDown)
      , (xK_comma  , sendMessage $ IncMasterN (-1))
      , (xK_period , sendMessage $ IncMasterN   1 )
      , (xK_d      , spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
      , (xK_g      , windowPrompt myXPromptConf Goto  allWindows)
      , (xK_b      , windowPrompt myXPromptConf Bring allWindows)
    --, (xK_x      , xmonadPrompt myXPromptConf)
      , (xK_space  , withFocused $ windows . StackSet.sink)
      , (xK_u      , myUWXPrompt conf) -- TODO move to FUA
      ] ++
      [ (wsKeySym  , windows $ (StackSet.greedyView . show) wsId) | Workspace{..} <- Set.toList myWorkspaces ]
              ++ ((, spawn "i3lock -n -c 000000") <$> Set.toList myLockScreenKeys)
              ++ ((, myXMonadSysPrompt) <$> Set.toList mySystemKeys)

    mySystemCommands :: Map KeySym (X ())
    mySystemCommands =
      [
        (xK_Up    , windows StackSet.swapUp)
      , (xK_Down  , windows StackSet.swapDown)
      , (xK_Left  , sendMessage FirstLayout)
      , (xK_Right , sendMessage NextLayout)       -- cycle through layouts
      , (xK_space , setLayout $ layoutHook conf)  -- reset layout
      , (xK_Left  , sendMessage Shrink)
      , (xK_Right , sendMessage Expand)
    --, (xK_r     , spawn $ "xmonad --recompile && " <> myXMonadRestart) -- TODO: recompilation not supported outside of nixos-rebuild atm
      , (xK_k     , spawn "xmodmap ~/.Xmodmap") -- TODO: make obsolete by defining udev rule
      ] ++
      [ (wsKeySym , windows $ (StackSet.shift . show) wsId) | Workspace{..} <- Set.toList myWorkspaces ]

    -- | Frequently used applications that can be launched via Mod+Shift+<key>
    myFUAs :: Map KeySym Application
    myFUAs = Map.fromList
      [
        -- Basic applications
        ( xK_t  -- [T]hunar
        , Application "thunar"
          mempty mempty mempty
        )
      , ( xK_k  -- [K]eepassxc password manager
        , Application "keepassxc"
          mempty mempty mempty
        )
      -- Web applications
      , ( xK_f  -- [F]irefox
        , Application "firefox"
          mempty mempty mempty
        )
      , ( xK_c  -- [C]hromium
        , Application "chromium"
          mempty [ "--disable-gpu-driver-bug-workarounds" ] mempty
        )
      , ( xK_m  -- -[M]ozilla Thunderbird
        , Application "thunderbird"
          (Map.singleton "LC_TIME" "en_DK.UTF-8") mempty mempty
        )
      -- Chat applications
      , ( xK_e  -- [E]lement desktop client (Matrix)
        , Application "element-desktop"
          mempty [ "--disable-gpu-driver-bug-workarounds" ] mempty
        )
      ]

myManageFloats :: ManageHook
myManageFloats = composeAll
  [ XMonad.appName =? "thunar"    --> doFloat
  , XMonad.appName =? "keepassxc" --> doFloat
  , XMonad.appName =? "nextcloud" --> doFloat
  , XMonad.appName =? "element-desktop" --> doFloat
  ]

myUWXPrompt :: XConfig Layout -> X ()
myUWXPrompt conf = xmonadPromptC (Map.toList myUWXPromptOpts) myUWXPromptConf where
  myUWXPromptConf :: XPConfig
  myUWXPromptConf = myXPromptConf
    { defaultPrompter = const "UniWorX >>= "
    , autoComplete    = Just 0
    , font = "-*-liberation sans mono-medium-r-normal--0-0-0-0-m-0-*-*"
    }
  myUWXPromptOpts :: Map String (X ())
  myUWXPromptOpts = Map.fromList
    [ ( "u[2w]: develop@srv01.uniworx.de:~/u2w" , spawn $ XMonad.terminal conf <> " --execute " <> myUWXUtilsDir <> "/launch-terminal/dev.sh --develop u2w" )
    , ( "f[radrive]: develop@srv01.uniworx.de:~/fradrive" , spawn $ XMonad.terminal conf <> " --execute " <> myUWXUtilsDir <> "/launch-terminal/dev.sh --develop fradrive" )
--  , ( "z[sh]: zsh@srv01.uniworx.de" , spawn $ XMonad.terminal conf <> " -e \"source " <> myUWXUtilsDir <> "launch-terminal/dev.sh --project u2w\"" )
--  , ( "n[ix-shell]: nix-shell@srv01.uniworx.de" , spawn $ XMonad.terminal conf <> " -e \"source " <> myUWXUtilsDir <> "launch-terminal/dev.sh --nix-shell u2w\"" )
--  , ( "m[onitor]: monitor servers", spawn $ XMonad.terminal conf <> " -e \"source " <> myUWXUtilsDir <> "monitor/all_servers.sh\"" )
--  , ( "l[ocal]: shell@localhost:~/u2w" , spawn $ XMonad.terminal conf <> " -e \"source " <> myUWXUtilsDir <> "launch-terminal/local.sh\"" )
--  , ( "s[shfs-]m[ount]: mount SSHFS" , spawn $ myUWXUtilsDir <> "sshfs/start.sh" )
--  , ( "s[shfs-]u[nmount]: unmount SSHFS" , spawn $ myUWXUtilsDir <> "sshfs/stop.sh" )
    ]
  myUWXUtilsDir = "~/.utils/uniworx"

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
  { ppCurrent = xmobarColor myFocusColor    mempty . wrap "[" "]"
  , ppHidden  = xmobarColor myMainColorDark mempty
  , ppUrgent  = xmobarColor myUrgentColor   mempty . wrap "!" "!"
  , ppSep     = " | "
  }


myModMask :: KeyMask
myModMask = mod4Mask  -- Mod == Super

myMainColorLight, myMainColorDark, myFocusColor, myUrgentColor :: String
myMainColorLight = "#fafafa"
myMainColorDark  = "#242424"
myFocusColor     = "#78aeed"
myUrgentColor    = "#ed333b"

myLayouts = 
              Mirror Grid
          ||| Tall nMaster delta frac
          ||| Mirror (Tall nMaster delta frac)
          ||| ThreeCol nMaster delta frac
          ||| Mirror (ThreeCol nMaster delta frac)
       -- ||| ResizableTall nMaster delta frac [1]
          ||| multiCol [1] nMaster delta frac
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
  { font         = "-*-liberation sans-medium-r-normal--0-0-0-0-p-0-0-*"
  , height       = 25
  , historySize  = 0
  , position     = Top
  , borderColor  = myMainColorDark
  , bgColor      = myMainColorDark
  , alwaysHighlight = True
  , bgHLight = myMainColorDark
  , fgHLight = myFocusColor
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



-- | Convenience definition for Mod1 (== Alt) key mask
altMask :: KeyMask
altMask = mod1Mask

-- | Convenience definition for the "no-key" mask (i.e. no key pressed)
noMask :: KeyMask
noMask = 0
