### Tamara and Jureks amazing haskell drawing machine
#### Usage:
```
project.exe [-g|--generations Int] [-a|--animate] [-c|--colorful]
            [-f|--frames Int] [-t|--target TARGET]
```
Executes a drawing/animation for the L-System specified in 'system.txt' or a given target file.

Available options:  
&nbsp;&nbsp; -g, --generations Int  &nbsp;&nbsp;   Number of generations. (default: 7)  
&nbsp;&nbsp;  -a, --animate &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Whether the drawing is animated or static.  
&nbsp;&nbsp;  -c, --colorful &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Life is beautiful.  
&nbsp;&nbsp;  -f, --frames Int   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Number of Frames drawn per second if program is started with '--animate'. (default: 20)  
&nbsp;&nbsp;  -t, --target TARGET   &nbsp;&nbsp; Target for the greeting (default: "system.txt")  
&nbsp;&nbsp;  -h, --help  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;Show this help text

The l-system file needs to be specified with file ending ("dragon.txt" instead of "dragon"). The file can't contain a newline after the last rule or the l-system will not be parsed.

#### Compiling:
Call 
```stack ghc project.hs```
with 'ParserCon.hs' and 'DrawerM.hs' in the same Folder.
