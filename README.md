# Sokoban game

```
stack build
stack exec sokoban
```
The aim is to build flexible testable model of the game, and make IO layer as thin as possible.

### Features to extend

1. Implement mouse events and reaction on `MoveBoxStart Point`,
    `MoveBoxEnd Point`, `MoveWorker Point`
1. `Config` and create monad transformers stack `ReaderT`
1. Include `RandomMonad` into the transformers stack
1. Parse command line options with `optparse-applicative`
