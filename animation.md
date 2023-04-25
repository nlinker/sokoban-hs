# How to make anomations in pure environment?

There are two variants to make animations in pure environment:
- Animation is reflected in the pure state, on each frame the state is changed accordingly the logic.
- Animation is the part of the impure environment, the state may or may not reflect the currently going animations.

So the main dichotomy is whether to make animations pure or impure.

## Pure animations

Pros:
1. The impure layer is thinner.
2. All animation frames are in sync with the actions from the keyboard and mouse.
3. The solver calculations can be immediately reused for step-by-step pathing.

Cons:
1. The state is polluted by the helper calculations and animation steps.
2. Performance is worse.

## Impure animations

Pros:
1. Better performance.
2. Better flexibility, the animation frames can be observed by pure code.
3. Better separation of abstractions.

Cons:
1. More impure code, less reproducible, more stuff to port if needed to do so.
2. More complex integration with the solver. The calculation should not only update 
    the state but to supply the neded information to the impure code as well. If
    we need the observability, then we need to provide the data flow backwards,
    i.e. from the animation code to the pure code.

Since I care about the performance and good abrtractions, I choose this, impure animations, approach.

## Animation implementation details

The cancellable animations is harder, so let's start with non-cancellable ones.

### Non-cancellable animations

```haskell
data Action
  = Move Direction
  | Undo
  | Redo
  | Restart
  | PrevLevel
  | NextLevel
  | SelectWorker    -- computeWorkerReachability
  | SelectBox Point -- computeBoxReachability box
  | MoveWorker Point -- moveWorkerToThePoint dst
  | MoveBox Point Point -- moveBoxByWorker src dst
  | ToggleDebugMode
  deriving (Eq, Show)
```
The longest operation here is `moveBoxByWorker src dst`, the animation