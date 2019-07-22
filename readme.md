### Tamara and Jureks amazing haskell drawing machine
Usage:
```
project.exe [-g|--generations Int] [-a|--animate] [-c|--colorful]
            [-f|--frames Int] [-t|--target TARGET]
```
Executes a drawing/animation for the L-System specified in 'system.txt' or a given target file.

Available options:
&nbsp;&nbsp; -g, --generations Int  &nbsp;&nbsp;   Number of generations. (default: 7)
&nbsp;&nbsp;  -a, --animate &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Whether the drawing is animated or static.
&nbsp;&nbsp;  -c, --colorful &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; Life is beautiful.
&nbsp;&nbsp;  -f, --frames Int  &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;Number of Frames drawn per second if program is
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;started with '--animate'. (default: 20)
&nbsp;&nbsp;  -t, --target TARGET   &nbsp;&nbsp; Target for the greeting (default: "system.txt")
&nbsp;&nbsp;  -h, --help &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;Show this help text
