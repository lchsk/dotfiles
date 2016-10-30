Config { 
	font = "xft:Inconsolata-9"
	--font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
       , bgColor = "#0f0f0f"
       , fgColor = "#c8c8c8"
       , position = BottomW L 100
       , commands = 
        [
          Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
          , Run Memory ["-t","Mem: <usedratio>%"] 10
          , Run Swap [] 10
          , Run Date "%a %b %_d %H:%M" "date" 10
          , Run StdinReader
          , Run CpuFreq [] 10
	  --, Run Com "/home/lchsk/.volume.sh" [] "vol" 30
	  --, Run Battery
          --[ "-t", "AC: <acstatus> <timeleft> (<left>%)"
          --, "-L", "10", "-H", "80", "-p", "3"
          --, "--", "-O", "<fc=green>[on]</fc>", "-o", "<fc=orange>[off]</fc>"
          --, "-L", "-15", "-H", "-5"
          --, "-l", "red", "-m", "cyan", "-h", "green"]
         --600
	--,Run Network "em1" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10	
	  --, Run DynNetwork [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
          --                   , "--Low"      , "1000"       -- units: kB/s
          --                   , "--High"     , "5000"       -- units: kB/s
          --                   , "--low"      , "darkgreen"
          --                   , "--normal"   , "darkorange"
          --                   , "--high"     , "darkred"
          --                   ] 10
	] 
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpufreq% | %cpu% | %memory% | %swap% | %date%"
