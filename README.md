# Cobble - A purely functional programming language that compiles to minecraft datapacks

## Installation
Right now, the only way to install cobble is to build it from source.

### Prerequisites
To install cobble, you need to have [haskell stack](https://docs.haskellstack.org/en/stable/README/) as well as [zlib](https://www.zlib.net/) installed.

### Build steps
First, clone the repository.
```bash
$ git clone https://github.com/Innf107/cobble-compiler
```
If you do not have git installed, just download the repository by clicking on **Code -> Download ZIP** and unzip the archive.

Once you have cloned the repository, build and install the project using stack. (This might take a while)
```bash
$ stack install
```
If everything compiled successfully, you should now get a message that will tell you where
the executable was installed. On Linux this is usually `~/.local/bin`.

Note that this is usually **not** on your PATH, so you probably want to add it or
copy the executable to a location on your PATH (like `/usr/bin` on Linux).

If you have no idea what I'm talking about, just google for: 
*set PATH \<insert your operating system\>*.

Once you're done, run `cobble --help` to check your installation. 



