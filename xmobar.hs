------------------------------------------------------------------------------
-- |
-- Copyright: (c) 2018, 2019, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 21:03
--
--
-- An example of a Haskell-based xmobar. Compile it with
--   ghc --make -- xmobar.hs
-- with the xmobar library installed or simply call:
--   xmobar /path/to/xmobar.hs
-- and xmobar will compile and launch it for you and
------------------------------------------------------------------------------

import Xmobar

-- Example user-defined plugin

data HelloWorld = HelloWorld
    deriving (Read, Show)

instance Exec HelloWorld where
    alias HelloWorld = "hw"
    run   HelloWorld = return "<fc=red>Hello Haskell!!</fc>"

-- Configuration, using predefined monitors as well as our HelloWorld
-- plugin:

config :: Config
config = defaultConfig {
  font = "xft:Sans Mono-9"
  , additionalFonts = []
  , borderColor = "black"
  , border = TopB
  , bgColor = "black"
  , fgColor = "grey"
  , alpha = 255
  , position = Top
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , allDesktops = True
  , overrideRedirect = True
  , textOutputFormat = Ansi
  , commands = [ Run $ Weather "EGPH" ["-t","<station>: <tempC>C",
                                        "-L","18","-H","25",
                                        "--normal","green",
                                        "--high","red",
                                        "--low","lightblue"] 36000
               , Run $ Network "eth0" ["-L","0","-H","32",
                                        "--normal","green","--high","red"] 10
               , Run $ Network "eth1" ["-L","0","-H","32",
                                        "--normal","green","--high","red"] 10
               , Run $ Cpu ["-L","3","-H","50",
                             "--normal","green","--high","red"] 10
               , Run $ Memory ["-t","Mem: <usedratio>%"] 10
               , Run $ Swap [] 10
               , Run $ Com "uname" ["-s","-r"] "" 36000
               , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
               , Run HelloWorld

               , Run $ Com "/home/rohits/.config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
               , Run $ Battery        [ "--template" , "Batt: <acstatus>"
                                         , "--Low"      , "10"        -- units: %
                                         , "--High"     , "80"        -- units: %
                                         , "--low"      , "darkred"
                                         , "--normal"   , "darkorange"
                                         , "--high"     , "darkgreen"

                                         , "--" -- battery specific options
                                                   -- discharging status
                                                   -- , "-o"	, "<left>% (<timeleft>)"
                                                   , "-o"   , "<left>%"
                                                   -- AC "on" status
                                                   , "-O"   , "<left>% <fc=#dAA520>Charging</fc>"
                                                   -- charged status
                                                   , "-i"   , "<fc=#006000>Charged</fc>"
                             ] 50

              ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%cpu% | %memory% * %swap% | %eth0% - %eth1% }\
               \ %hw% { <fc=#ee9a00>%date%</fc>| %battery% | %uname% | %trayerpad% "
}

main :: IO ()
main = xmobar config
