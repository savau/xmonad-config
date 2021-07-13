module Applications
  ( myStartupApplications, myFUAs
  , spawnApplication
  ) where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)

import XMonad

import XMonad.Actions.SpawnOn (spawnOn)


type Application            = String
type ApplicationEnvironment = Map String String
type ApplicationOptions     = [String]


-- | Applications that will be automatically launched after starting XMonad
myStartupApplications :: Map Application (ApplicationEnvironment, ApplicationOptions, Maybe WorkspaceId)
myStartupApplications = Map.fromList
  [ ("xfce4-power-manager" , ( mempty, mempty, mempty    ) )
  , ("volumeicon"          , ( mempty, mempty, mempty    ) )
  , ("nm-applet"           , ( mempty, mempty, mempty    ) )
  , ("blueman-applet"      , ( mempty, mempty, mempty    ) )
--, ("pamac-tray"          , ( mempty, mempty, mempty    ) )  -- TODO: launch this iff on Arch Linux
  , ("keepassxc"           , ( mempty, mempty, mempty    ) )
  , ("megasync"            , ( Map.fromList [ ("QT_SCALE_FACTOR","1") ], mempty, mempty ) )  -- setting QT_SCALE_FACTOR=1 as a workaround to avoid immediate segfault, see https://github.com/meganz/MEGAsync/issues/443
  , ("birdtray"            , ( Map.fromList [ ("LC_TIME","root.UTF-8") ], mempty, mempty ) )
--, ("thunderbird"         , ( mempty, mempty, Just "10" ) )
--, ("zulip"               , ( mempty, mempty, Just "9"  ) )
--, ("signal-desktop-beta" , ( mempty, mempty, Just "8"  ) )
  ]

-- | Frequently used applications that can be launched via Mod+Shift+<key>
myFUAs :: Map KeySym String
myFUAs = Map.fromList
  [ (xK_f, "thunar"                         )  -- file manager
  , (xK_k, "keepassxc"                      )  -- password manager
  , (xK_w, "firefox"                        )  -- web browser
  , (xK_m, "LC_TIME=root.UTF-8 thunderbird" )  -- mail client
  , (xK_z, "zulip"                          )  -- chat client
  , (xK_c, "google-calendar-dark"           )  -- calendar client
  , (xK_s, "signal-desktop"                 )  -- messenger client
--, (xK_p, "pidgin"                         )  -- XMPP client
  , (xK_t, "texstudio"                      )  -- tex editor
  , (xK_o, "octave --gui"                   )  -- GNU Octave
  , (xK_j, "idea"                           )  -- IntelliJ IDEA
  ]


-- | Spawns a given application with given options and optionally on a given workspace
spawnApplication :: Application -> ApplicationEnvironment -> ApplicationOptions -> Maybe WorkspaceId -> X ()
spawnApplication app env opts mWorkspace = maybe spawn spawnOn mWorkspace $ intercalate " " $ (fmap (\(k,v) -> k<>"="<>v<>" ") $ Map.toList env) <> (app:opts)
