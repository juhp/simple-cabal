# Changelog

`simple-cabal` uses [PVP Versioning](https://pvp.haskell.org)

## 0.1.0 (2019-09-29)
- add buildDependencies and testsuiteDependencies (from cabal-rpm)
- and rest of deps compatibility from cabal-rpm with fixes
- export prettyShow, allBuildInfo, BuildInfo, FlagName & mkFlagName,
  setupDepends
- no longer export: normal, showPkgId, unPackageName
- support Cabal-3 (ghc-8.8.1)

## 0.0.0.1 (2019-07-09)
- allow building with Cabal 1!

## 0.0.0 (2019-06-08)
- Initially version, mostly taken from cabal-rpm PackageUtils.hs
