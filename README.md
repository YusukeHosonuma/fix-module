# fix-module

A CLI tool that auto-fix module name of .hs files from the definition of `package.yaml`.

```bash
$ cd <stack-project-root>
$ fix-module           # Warning: This command will overwrite .hs files
$ fix-module --verbose # With verbose log
```

## TODO
- [ ] Add support to `.cabal`.
- [ ] Auto-fix import. (undecided)

## Installation

### Homebrew

```bash
$ brew tap YusukeHosonuma/fix-module
$ brew install fix-module
```

### Manual

```bash
$ stack install
$ ~/.local/bin/fix-module # or add `~/.local/bin` to PATH environment
```

## Development

Requirements:

- [Stack](https://docs.haskellstack.org/en/stable/README/) 2.5.1
- [GHC](https://www.haskell.org/ghc/) 8.10.3

```bash
$ stack build
$ stack test
```

## Issues and PR

You have any ideas or found bugs? Any issue or PR is welcome.<br>
Thanks!

## Author

[Yusuke Hosonuma](https://github.com/YusukeHosonuma) / [@tobi462](https://twitter.com/tobi462)
