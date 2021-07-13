module Applications
  ( myStartupApplications, myFUAs
  , spawnApplication
  ) where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)

import XMonad

import XMonad.Actions.SpawnOn (spawnOn)


type Application = String

type ApplicationOptions = [String]


-- | Applications that will be automatically launched after starting XMonad
-- TODO: use `Map ([String], Maybe WorkspaceId)` to avoid duplicate launches
myStartupApplications :: [(Application, ApplicationOptions, Maybe WorkspaceId)]
myStartupApplications =
  [ ("xfce4-power-manager"         , mempty         , mempty    )
  , ("volumeicon"                  , mempty         , mempty    )
  , ("nm-applet"                   , mempty         , mempty    )
  , ("blueman-applet"              , mempty         , mempty    )
--, ("pamac-tray"                  , mempty         , mempty    )  -- TODO: launch this iff on Arch Linux
  , ("keepassxc"                   , mempty         , mempty    )
  , ("QT_SCALE_FACTOR=1 megasync"  , mempty         , mempty    )  -- setting QT_SCALE_FACTOR=1 as a workaround to avoid immediate segfault, see https://github.com/meganz/MEGAsync/issues/443
  , ("LC_TIME=root.UTF-8 birdtray" , mempty         , mempty    )
--, ("thunderbird"                 , mempty         , Just "10" )
--, ("zulip"                       , mempty         , Just "9"  )
--, ("signal-desktop-beta"         , mempty         , Just "8"  )
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
spawnApplication :: Application -> ApplicationOptions -> Maybe WorkspaceId -> X ()
spawnApplication app opts mWorkspace = maybe spawn spawnOn mWorkspace $ intercalate " " $ app : opts
