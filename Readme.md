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
pj_exe_path=~/.local/bin/pj-haskell-exe
pj () {
    pj_output=$("$pj_exe_path" $*)
    pj_exit_code=$?
    if [[ ( "$pj_exit_code" -eq 0 ) && ( $(echo "$pj_output" | wc -l) -eq 1 ) && ( "$pj_output" == /* ) ]]; then
        if [ -d "$pj_output" ]; then
            cd "$pj_output"
            return 0
        else
            echo "$pj_output" "is not a directory!"
            return 2
        fi
    else
        echo "$pj_output"
        return $pj_exit_code
    fi
}
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

## Exit codes

- `0`: successfully done the command
- `2`: label path is not a directory
- `4`: given project name is invalid
- `8`: there is no label with given name
- `16`: incorrect usage of a command (wrong arguments count, etc.)

## Additional info

This program stores all labels inside `~/.pj` file. It will be silently created if not exists.

Also program relies on fact that file `~/.pj.temp` does not exist.
