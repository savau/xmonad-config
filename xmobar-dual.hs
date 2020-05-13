Config { font = "xft:Droid Sans Mono-8:antialias=true"
       , additionalFonts = []
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 255
       , position = Static { xpos = 0, ypos = 0, width = 1815, height = 20 },
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Weather "EDDM" ["-t","<station>: <tempC>°C <skyCondition>",
                                          "-L","15","-H","25",
                                          "--normal","#d1d173",
                                          "--high","red",
                                          "--low","lightblue"] 18000
                    --, Run Network "enp0s31f6" ["-t",
                    --                      "<dev>: up <rx><rxipat> down <tx><txipat>",
                    --                      "-L","0","-H","32",
                    --                      "--low","green",
                    --                      "--normal","yellow","--high","red"] 10
                    --, Run Wireless "wlp0s20f3" ["-t", "WLAN: <essid> <quality>%"] 10
                    , Run DynNetwork ["-t","<dev>: rx <rx>, tx <tx>","-S","True"] 50
                    , Run Wireless "wlp0s20f3" ["-t","<essid>: <quality>%",
                                      "-L","30","-H","60",
                                      "--low","red","--normal","yellow",
                                      "--high","green"
                                     ] 50
                    , Run MultiCpu ["-t","Cpu: <autototal>",
                                    "-L","50","-H","90",
                                    "--low","green",
                                    "--normal","yellow","--high","red"] 25
                    , Run CoreTemp ["-t", "<core0>°C",
                                    "-L","60","-H","80",
                                    "--low","green","--normal","yellow",
                                    "--high","red"] 50
                    , Run Memory ["-t","Mem: <usedratio>%","--low","green",
                                  "--normal","yellow","--high","red"] 10
                    , Run Swap ["--low","green","--normal","yellow","--high","red"] 10
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Date "%Y-%m-%d %H:%M:%S" "date" 10
                    , Run Battery ["-t","Bat: <left>%",
                                   "-L","15","-H","70",
                                   "--low","red","--normal","yellow",
                                   "--high","green"] 50
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %multicpu% - %coretemp% | %memory% - %swap% | %dynnetwork% - %wlp0s20f3wi% | %battery% | %uname%    %EDDM% | <fc=#ffffff>%date%</fc> | "
       }
