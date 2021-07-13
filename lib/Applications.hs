{-# LANGUAGE RecordWildCards #-}

module Applications
  ( Application(..)
  , spawnApplication
  , myStartupApplications, myFUAs
  ) where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import XMonad

import XMonad.Actions.SpawnOn (spawnOn)


data Application = Application
                   { appName        :: String
                   , appEnvironment :: Map String String
                   , appOptions     :: [String]
                   , appWorkspace   :: Maybe WorkspaceId
                   }
                   deriving (Eq, Ord, Show, Read)

-- | Spawns a given application with the given environment and options, if given on a specific workspace
spawnApplication :: Application -> X ()
spawnApplication Application{..} = maybe spawn spawnOn appWorkspace $ intercalate " " $ (fmap (\(k,v) -> k<>"="<>v<>" ") $ Map.toList appEnvironment) <> (appName : appOptions)


-- | Applications that will be automatically launched after starting XMonad
myStartupApplications :: Set Application
-- Map Application (ApplicationEnvironment, ApplicationOptions, Maybe WorkspaceId)
myStartupApplications = (Set.fromList . fmap (\(appName,appEnvironment,appOptions,appWorkspace) -> Application{..}))
  [ ("xfce4-power-manager" , mempty, mempty, mempty    )
  , ("volumeicon"          , mempty, mempty, mempty    )
  , ("nm-applet"           , mempty, mempty, mempty    )
  , ("blueman-applet"      , mempty, mempty, mempty    )
--, ("pamac-tray"          , mempty, mempty, mempty    )  -- TODO: launch this iff on Arch Linux
  , ("keepassxc"           , mempty, mempty, mempty    )
  , ("megasync"            , Map.fromList [ ("QT_SCALE_FACTOR","1") ], mempty, mempty )  -- setting QT_SCALE_FACTOR=1 as a workaround to avoid immediate segfault, see https://github.com/meganz/MEGAsync/issues/443
  , ("birdtray"            , Map.fromList [ ("LC_TIME","root.UTF-8") ], mempty, mempty )
--, ("thunderbird"         , mempty, mempty, Just "10" )
--, ("zulip"               , mempty, mempty, Just "9"  )
--, ("signal-desktop-beta" , mempty, mempty, Just "8"  )
  ]

-- | Frequently used applications that can be launched via Mod+Shift+<key>
myFUAs :: Map KeySym Application
myFUAs = Map.fromList
  [ ( xK_f, Application "thunar"         mempty mempty mempty )  -- file manager
  , ( xK_k, Application "keepassxc"      mempty mempty mempty )  -- password manager
  , ( xK_w, Application "firefox"        mempty mempty mempty )  -- web browser
  , ( xK_m, Application "thunderbird"    (Map.singleton "LC_TIME" "root.UTF-8") mempty mempty )  -- mail client
  , ( xK_z, Application "zulip"          mempty mempty mempty )  -- chat client
--, ( xK_c, Application "google-calendar-dark" mempty mempty mempty )  -- calendar client
  , ( xK_s, Application "signal-desktop" mempty mempty mempty )  -- messenger client
--, ( xK_p, Application "pidgin" mempty mempty mempty )  -- XMPP client
  , ( xK_t, Application "texstudio"      mempty mempty mempty )  -- tex editor
  , ( xK_o, Application "octave"         mempty ["--gui"] mempty )  -- GNU Octave
  , ( xK_j, Application "idea"           mempty mempty mempty )  -- IntelliJ IDEA
  ]
