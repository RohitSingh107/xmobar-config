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

import           Xmobar

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
  font = "xft:Sans Mono-12"
  , additionalFonts = []
  , borderColor = "black"
  , border = TopB
  , bgColor = "#D98695"
  , fgColor = "#ffffff"
  -- , alpha = 130
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

               , Run $ Cpu ["-t", "  cpu: (<fc=#000000,#fb9f7f:0><total>%</fc>)","-H","50","--high","red"] 20
               , Run $ Memory ["-t","Mem: <usedratio>%"] 10
               , Run $ Memory ["-t", "  mem: <fc=#000000,#56b6c2:0><used></fc>M (<fc=#000000,#56b6c2:0><usedratio>%</fc>)"] 20
               , Run $ Swap [] 10
               , Run $ Com "uname" ["-s","-r"] "" 36000
               , Run $ Date "  %b %d %Y - (<fc=#000000,#c678dd:0>%I:%M</fc>) " "date" 50
               , Run HelloWorld
               , Run $ UnsafeStdinReader
               , Run $ DiskU [("/", " hdd: <fc=#000000,#29AB87:0><free></fc> free")] [] 60

               , Run $ Com "/home/rohits/.config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
               , Run $ Battery        [ "--template" , "Batt: <acstatus>"
                                         , "--Low"      , "10"        -- units: %
                                         , "--High"     , "80"        -- units: %
                                         , "--low"      , "#000000,#e75480:0"
                                         , "--normal"   , "#000000,#e75480:0"
                                         , "--high"     , "#000000,#e75480:0"

                                         , "--" -- battery specific options
                                                   -- discharging status
                                                   -- , "-o"	, "<left>% (<timeleft>)"
                                                   , "-o"   , "<fc=#000000,#e75480:0><left>%</fc>"
                                                   -- AC "on" status
                                                   , "-O"   , "<left>% <fc=#000000,#e75480:0>Charging</fc>"
                                                   -- charged status
                                                   , "-i"   , "<fc=#000000,#e75480:0>Charged</fc>"
                             ] 50
              ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "<fc=#000000,#fb9f7f:0><action=`alacritty -e htop`>%cpu%</action></fc>\
			   \<fc=#000000,#29AB87:0><action=`gparted`>%disku%</action></fc>\
               \<fc=#000000,#56b6c2:0><action=`alacritty -e htop`>%memory%</action></fc>  }\
               \%UnsafeStdinReader% { \
			   \<fc=#000000,#e75480:0><action=`stacer`>%battery%</action></fc>\
			   \<fc=#000000,#c678dd:0><action=`gsimplecal`>%date%</action></fc>\
			   \%trayerpad% "
}

main :: IO ()
main = xmobar config
