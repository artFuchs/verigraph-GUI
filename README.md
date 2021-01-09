# Verigraph-GUI

A graph-grammar editor that's intended to be used in the [verigraph tool](https://github.com/Verites/verigraph) in the future.
For now, it can export a .ggx file (default file format for AGG).
The editor in this repository makes use of the module Data.Graphs from the Verigraph source.

# Installation

## Dependences

Verigraph-GUI uses the bindings of [haskell-gi](https://github.com/haskell-gi/haskell-gi).
To run a gtk+3 progrma, you must make sure you have the right development packages for the libs.

For **Debian/Ubuntu**:
```bash
  $ sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
```

For **Arch Linux**:
```bash
  $ sudo pacman -S gobject-introspection gobject-introspection-runtime gtksourceview3 webkit2gtk
```

Examples for other distros can be found in the [github page of haskell-gi](https://github.com/haskell-gi/haskell-gi).

## Stack Tool

For install, it's recommended to use the [stack tool](https://docs.haskellstack.org/en/stable/README/).


You must make sure the `stack` version is **1.6 or later**.
You can check it with
```bash
  $ stack --version
```

If it's older, you can upgrade `stack` with
```bash
  $ stack upgrade
  $ echo "export PATH=~/.local/bin:${PATH}" >> ~/.bashrc
  $ source ~/.bashrc
```
You can then remove the previous stack package.

Then, to install verigraph-GUI, go to the directory where you cloned this repository and run

```bash
  $ stack install
```

You should also copy the folder `./app/Resources/` to `~/.local/share/verigraph-GUI/`, as verigraph-GUI need these files to open. It's optional, but if this is not done you must execute verigraph-GUI in a folder that contains `Resources/`

```bash
  $ mkdir -r ~/.local/share/verigraph-GUI/
  $ cp -r ./app/Resources/ ~/.local/share/verigraph-GUI/
```

Then to run it, use the command
```bash
  $ verigraph-GUI
```
