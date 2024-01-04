# Optimization levels

All of these are done via `rm -rf dist-newstyle && cabal build` to ensure
everything is rebuilt.

## Opt 0

`cabal.project`:

```cabal
package *
  optimization: 0
  split-sections: true
  executable-stripping: true
  library-stripping: true
  library-for-ghci: false
```

Build time:

Run time:

```console
cabal run HaskellRT -- +RTS '-s'
```

## Opt 1

`cabal.project`:

```cabal
package *
  optimization: 1
  split-sections: true
  executable-stripping: true
  library-stripping: true
  library-for-ghci: false
```

Build time:

Run time:

```console
cabal run HaskellRT -- +RTS '-s'
```

## Opt 2

`cabal.project`:

```cabal
package *
  optimization: 2
  split-sections: true
  executable-stripping: true
  library-stripping: true
  library-for-ghci: false
```

Build time:

Run time:

```console
cabal run HaskellRT -- +RTS '-s'
```

### With flags (default)

`cabal.project`:

```cabal
package *
  optimization: 2
  split-sections: true
  executable-stripping: true
  library-stripping: true
  library-for-ghci: false
  ghc-options:
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--feager-blackholing
    -feager-blackholing
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fspecialise-aggressively
    -fspecialise-aggressively
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fexpose-all-unfoldings
    -fexpose-all-unfoldings
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--flate-dmd-anal
    -flate-dmd-anal
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fmax-simplifier-iterations=⟨n⟩
    -fmax-simplifier-iterations=4
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fsimplifier-phases=⟨n⟩
    -fsimplifier-phases=2
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fsimpl-tick-factor=⟨n⟩
    -fsimpl-tick-factor=100
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--flate-specialise
    -flate-specialise
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fstatic-argument-transformation
    -fstatic-argument-transformation
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/applicative_do.html#ghc-flag--foptimal-applicative-do
    -foptimal-applicative-do
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using.html#ghc-flag--mfma
    -mfma
```

Build time:

Run time:

```console
cabal run HaskellRT -- +RTS '-s'
```

### With flags (4x default settings for simplifier)

`cabal.project`:

```cabal
package *
  optimization: 2
  split-sections: true
  executable-stripping: true
  library-stripping: true
  library-for-ghci: false
  ghc-options:
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--feager-blackholing
    -feager-blackholing
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fspecialise-aggressively
    -fspecialise-aggressively
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fexpose-all-unfoldings
    -fexpose-all-unfoldings
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--flate-dmd-anal
    -flate-dmd-anal
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fmax-simplifier-iterations=⟨n⟩
    -fmax-simplifier-iterations=16
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fsimplifier-phases=⟨n⟩
    -fsimplifier-phases=8
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fsimpl-tick-factor=⟨n⟩
    -fsimpl-tick-factor=400
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--flate-specialise
    -flate-specialise
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fstatic-argument-transformation
    -fstatic-argument-transformation
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/applicative_do.html#ghc-flag--foptimal-applicative-do
    -foptimal-applicative-do
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using.html#ghc-flag--mfma
    -mfma
```

Build time:

Run time:

```console
cabal run HaskellRT -- +RTS '-s'
```

### With flags (16x default settings for simplifier)

`cabal.project`:

```cabal
package *
  optimization: 2
  split-sections: true
  executable-stripping: true
  library-stripping: true
  library-for-ghci: false
  ghc-options:
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--feager-blackholing
    -feager-blackholing
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fspecialise-aggressively
    -fspecialise-aggressively
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fexpose-all-unfoldings
    -fexpose-all-unfoldings
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--flate-dmd-anal
    -flate-dmd-anal
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fmax-simplifier-iterations=⟨n⟩
    -fmax-simplifier-iterations=64
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fsimplifier-phases=⟨n⟩
    -fsimplifier-phases=32
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fsimpl-tick-factor=⟨n⟩
    -fsimpl-tick-factor=1600
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--flate-specialise
    -flate-specialise
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fstatic-argument-transformation
    -fstatic-argument-transformation
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/applicative_do.html#ghc-flag--foptimal-applicative-do
    -foptimal-applicative-do
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using.html#ghc-flag--mfma
    -mfma
```

Build time:

Run time:

```console
cabal run HaskellRT -- +RTS '-s'
```

### With flags (stupid large settings for simplifier)

`cabal.project`:

```cabal
package *
  optimization: 2
  split-sections: true
  executable-stripping: true
  library-stripping: true
  library-for-ghci: false
  ghc-options:
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--feager-blackholing
    -feager-blackholing
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fspecialise-aggressively
    -fspecialise-aggressively
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fexpose-all-unfoldings
    -fexpose-all-unfoldings
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--flate-dmd-anal
    -flate-dmd-anal
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fmax-simplifier-iterations=⟨n⟩
    -fmax-simplifier-iterations=50000
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fsimplifier-phases=⟨n⟩
    -fsimplifier-phases=10000
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fsimpl-tick-factor=⟨n⟩
    -fsimpl-tick-factor=100000
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--flate-specialise
    -flate-specialise
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#ghc-flag--fstatic-argument-transformation
    -fstatic-argument-transformation
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/applicative_do.html#ghc-flag--foptimal-applicative-do
    -foptimal-applicative-do
    -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using.html#ghc-flag--mfma
    -mfma
```

Build time:

Run time:

```console
cabal run HaskellRT -- +RTS '-s'
```
