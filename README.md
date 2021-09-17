# gd-blueprints

This tool makes blacksmith recipes you found in either game mode (hardcore/softcore) available for both game modes.

It will not automatically keep synchronizing your recipes between game modes, you need to rerun the application each time their state changes.

Make a backup of your save files before using this, also make sure you are not running grim dawn when executing this application.

> Warning: this project has no tests =)

### Usage

Run the `gd-blueprints` in the game save folder  which contains both `formulas.gsh` and `formulas.gst` files.

For example:
```bash
$ cd "$HOME/Documents/My Games/Grim Dawn/save"
$ gd-blueprints
```

### Building from source

Requires `stack` (Check https://www.haskellstack.org/ for install instructions)

if you want to clone de repository, you may need to install `git` too (https://git-scm.com/)

Clone/Download and unzip this repository, `cd` into it and run `stack install`

for example:
```bash
$ git clone https://github.com/jean-lopes/grim-dawn-blueprints.git
$ cd grim-dawn-blueprints
$ stack install
```

