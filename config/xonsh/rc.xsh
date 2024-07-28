# The SQLite history backend saves command immediately
# unlike JSON backend that save the commands at the end of the session.
# https://xon.sh/envvars.html#histcontrol
$XONSH_HISTORY_FILE = $XDG_DATA_HOME + "/xonsh/history.sqlite"
$XONSH_HISTORY_BACKEND = "sqlite"
$HISTCONTROL = "erasedups"

# I'm not a xonsh developer, I don't care about stack traces.
$XONSH_SHOW_TRACEBACK = False

# HACK: https://github.com/NixOS/nixpkgs/issues/276326
$PATH = [
    path for path in $PATH
    if not ((p"" / path / "xonsh").exists() and (p"" / path).parts[1] == "nix")
]

# HACK: https://github.com/xonsh/xonsh/issues/3744
__xonsh__.commands_cache.threadable_predictors['cat'] = lambda *a, **kw: True

import json
aliasFile = p"$XDG_CONFIG_HOME/xonsh/aliases.json"
if aliasFile.exists():
    for alias, expansion in json.loads(aliasFile.read_text()).items():
        aliases[alias] = expansion

for i in range(1, 10):
    aliases["." + ("." * i)] = "cd " + ("../" * i)

def ns(args):
    args = [x if x[0:2] == "--" else f"nixpkgs#{x}" for x in args]
    @(["nom", "shell", *args, "--command", "xonsh"])

aliases["ns"] = ns
del ns

# def prompt_left():
#     return "{cwd} {prompt_end} "

# def prompt_right():
#     return ""

def prompt_bottom():
    ret = ""

    # date and time
    ret += "[ " + $(date '+%a %Y-%m-%d %H:%M') + " ] "

    # battery
    bat_cmd = !(cat /sys/class/power_supply/BAT*/capacity)
    bat_cmd.end()
    if bat_cmd.returncode == 0:
        bat = (lambda x: round(sum(x) / len(x)))(
            [int(x) for x in bat_cmd.strip().split("\n")]
        )
        ret += f"[ BAT: {bat:02}% ] "

    return ret + (" " * 2048)

# $PROMPT = prompt_left
# $RIGHT_PROMPT = prompt_right
$BOTTOM_TOOLBAR = prompt_bottom

@carapace_init@
@zoxide_init@
@direnv_init@

