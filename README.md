# graph-editor

A graph-grammar editor that's intended to be used in the [verigraph tool](https://github.com/Verites/verigraph) in the future.
For now, it can export a .ggx file (default file format for AGG).
The editor in this repository makes use of the module Data.Graphs from the Verigraph source.


# Usage Instructions

For install, it's recommended to use the [stack tool](https://docs.haskellstack.org/en/stable/README/).

```bash
  $ stack install
```

Then to run it, use the command
```bash
  $ graph-editor-exe
```

## important
To run the editor, the program must be executed in the same level of the "app/Resources" folder, as it has the glade files used to build the main window. 
You may want to copy the "Resources" folder to your workplace.
This is a problem that will be fixed later.
