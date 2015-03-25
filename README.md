# bookmark-manager
Haskell cmdl bookmark manager

Useful unix cmds:
* sort by tag frequency: `cabal run -- -t | grep -o "[a-z\-]*\|[0-9]*" | sed 'N;s/\n/ /' | sort -n -k 2`
