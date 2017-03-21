# `pj`

## Description

`pj` is a simple command-line utility for managing and navigating filesystem labels.

A __filesystem label__ (label) is a combination of its _name_ and a _path_ to a directory.
Name of the label is a plain string (containing no whitespace).
Path is a string representing a directory path in your filesystem.

## Installation

You need a configured [haskell-stack](https://docs.haskellstack.org/en/stable/README/) to install this program.

```bash
git clone git@github.com:BobNobrain/pj-haskell.git
cd pj-haskell
stack build
stack install
```

Next, add this to your `~/.bashrc` file:

```bash
# (TODO)
```

## Usage

```bash
pj --help # prints usage and short commands description
pj <cmd> --help # prints command description
```

Available commands are:

- `pj add <name> <path>` - creates a new label with name &lt;name&gt;, pointing to directory at &lt;path&gt;;
- `pj rm <name1>[ <name2>[ ...]]` - removes labels with given names;
- `pj list` - lists all available labels;
- `pj <name>` - if `pj` is added to your `~/.bashrc` as described in _Installation_, will change your working directory to the one that was labelled &lt;name&gt;.

## Additional info

This program stores all labels inside `~/.pj` file. It will be silently created if not exists.

Also program relies on fact that file `~/.pj.temp` does not exist.
