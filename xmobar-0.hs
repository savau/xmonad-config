Config
  {
    font             = "xft:Droid Sans Mono-8:antialias=true"
  , additionalFonts  = []     -- ^ List of alternative fonts
--, wmClass :: String         -- ^ X11 WM_CLASS property value
--, wmName  :: String         -- ^ X11 WM_NAME property value
  , bgColor          = "black"
  , fgColor          = "grey"
  , position         = Static { xpos = 0, ypos = 0, width = 1790, height = 20 }
  , textOffset       = -1
  , textOffsets      = []     -- ^ List of offsets for additionalFonts
  , iconOffset       = -1
  , border           = TopB
  , borderColor      = "black"
  , borderWidth      = 0
  , alpha            = 255
  , hideOnStart      = False
  , allDesktops      = True
  , overrideRedirect = True   -- ^ Needed for dock behaviour in some non-tiling WMs
  , pickBroadest     = False  -- ^ Use the broadest display instead of the first one
                              --   by default
  , lowerOnStart     = True   -- ^ Lower to the bottom of the window stack 
                              --   on initialization
  , persistent       = False  -- ^ Toggle automatic hiding
  , iconRoot         = "."
  , commands         =
    [ 
      Run Weather "EDDM"
        [ "-t"       , "<station>: <tempC>°C <skyCondition>"
        , "-L"       , "15" 
        , "-H"       , "25"
        , "--normal" , "#d1d173"
        , "--high"   , "red"
        , "--low"    , "lightblue"
        ] 18000
    , Run DynNetwork
        [ "-t"       , "<dev>: rx <rx>, tx <tx>"
        , "-S"       , "True"
        ] 50
    , Run Wireless "wlp0s20f3"
        [ "-t"       , "<essid>: <quality>%"
        , "-L"       , "30"
        , "-H"       , "60"
        , "--low"    , "red"
        , "--normal" , "yellow"
        , "--high"   , "green"
        ] 50
    , Run MultiCpu
        [ "-t"       , "Cpu: <autototal>"
        , "-L"       , "50"
        , "-H"       , "90"
        , "--low"    , "green"
        , "--normal" , "yellow"
        , "--high"   , "red"
        ] 25
    , Run CoreTemp
        [ "-t"       , "<core0>°C"
        , "-L"       , "60"
        , "-H"       , "80"
        , "--low"    , "green"
        , "--normal" , "yellow"
        , "--high"   , "red"
        ] 50
    , Run Memory
        [ "-t"       , "Mem: <usedratio>%"
        , "--low"    , "green"
        , "--normal" , "yellow"
        , "--high"   , "red"
        ] 10
    , Run Swap
        [ "--low"    , "green"
        , "--normal" , "yellow"
        , "--high"   , "red"
        ] 10
    , Run Com "uname"
        [ "-s"       , "-r"
        ] "" 36000
    , Run Date "%Y-%m-%d %H:%M:%S" "date" 10
    , Run Battery
        [ "-t"       , "Bat: <left>%"
        , "-L"       , "15"
        , "-H"       , "70"
        , "--low"    , "red"
        , "--normal" , "yellow"
        , "--high"   , "green"
        ] 50
    , Run StdinReader
    ]
  , sepChar          = "%"
  , alignSep         = "}{"
  , template         = " %StdinReader% }{ %multicpu% - %coretemp% | %memory% - %swap% | %dynnetwork% - %wlp0s20f3wi% | %battery% | %uname%        %EDDM% | <fc=#ffffff>%date%</fc> | "
  , verbose          = False  -- ^ Emit additional debug messages
  }
