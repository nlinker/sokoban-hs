# Sokoban game

The goal is to build flexible testable model of the game, and make IO layer as thin as possible.

Also the game should implement advanced move-ability control with reachability test, and such algorithms
are usually hard to implement in Haskell, but in this project it is achieved nevertheless, see
[`Solver.hs`](src/Sokoban/Solver.hs) module.

https://raw.githubusercontent.com/nlinker/sokoban-hs/master/docs/demo.mp4

### Installation and run
                        
You need to have [Stack](https://docs.haskellstack.org/en/stable/) installed.

```
stack build
stack exec sokoban
```

### Requests for the improvement. 

1. ☑ Track `steps` and `pushes`.
1. ☑ Implement mouse events and reaction on `MoveBox Point Point`, `MoveWorker Point`
    (this is pretty hard, since it requires path finding algorithm implemented for with
    pushes or steps optimization).
1. ☑ Parse command line options with `optparse-applicative`.
1. ⍻ Replays and stored solutions.
1. ⍻ Local storage to store solved/unsolved, progress and score board.
1. ⍻ Solver, that tries to find the solution from the current position
    (should be cancellable).
1. ⍻ Deadlock detection (similar to the feature above, but can be done 
    with brute-force algorithm).
1. ⍻ Application configuration and monad transformers stack `ReaderT` with it.
1. ⍻ Include `RandomMonad` into the transformers stack to randomly select 
    the next level.
1. ⍻ Implement a web version for the game, i.e. write the REST interface 
    using [servant](https://github.com/haskell-servant/servant).
1. ⍻ Write a frontend in ELM language.
1. ⍻ Write TUI using [brick](https://github.com/jtdaugherty/brick).


### Where to get sokoban levels

- http://www.sourcecode.se/sokoban/levels
- https://sokoban-jd.blogspot.com/p/all-my-sokoban-collections.html
- https://sokoban-game.com/packs
- http://www.sneezingtiger.com/sokoban/levels.html

### Info on sokoban solvers

http://sokobano.de/wiki/index.php?title=Solver
https://www.sokoban-online.de/

### Haskell on SO

https://stackoverflow.com/questions/51874362/combine-st-and-list-monads-in-haskell
https://stackoverflow.com/questions/11662696/haskell-map-runst
https://stackoverflow.com/questions/12468622/how-does-the-st-monad-work
https://stackoverflow.com/questions/9468963/runst-and-function-composition
https://stackoverflow.com/questions/48727762/what-is-an-elegant-idiom-for-a-lexicographic-ord-instance
https://stackoverflow.com/questions/3651144/comparing-lists-in-haskell-or-more-specifically-what-is-lexicographical-order
https://stackoverflow.com/questions/38629806/get-the-minimum-value
https://stackoverflow.com/questions/18006845/function-in-haskell-that-like-catmaybes-but-having-type-maybe-a-maybe-a
https://stackoverflow.com/questions/27399696/filter-positions-in-a-list-haskell




### Useful information

- [How to get MouseMove and MouseClick in bash](https://stackoverflow.com/a/5970472/5066426)
- [Simple XLib program to highlight the cursor position](https://github.com/arp242/find-cursor)
- [Handling Control-C in Haskell](https://neilmitchell.blogspot.com/2015/05/handling-control-c-in-haskell.html?m=1)
- [STMonadTrans](https://hackage.haskell.org/package/STMonadTrans) A monad transformer version of the ST monad
- [Sokolution author's thoughts](http://sokobano.de/wiki/index.php?title=Sokoban_solver_%22scribbles%22_by_Florent_Diedler_about_the_Sokolution_solver)
- [Mutable hashtables library](https://hackage.haskell.org/package/hashtables-1.2.3.4) and its author announcing it

- Unicode symbols to try for walls, goals, boxes and worker:

```
⬛ ■ ◼ ◾ ▪ □ ⬚ ▫ ◻ ❎ ⬛ ⬜ ▢ ▣ ◽ ❑ ❒ ❏ ❐ ▩ ▦ ▧ ▨ ⊞ ⊡ ☒
⊕ ⊗ ✪ ⊙ ⦾ ⦿ ⊚ ⊛ ○ ◌ ● ◯ ⬤ ⌾ ⍟ ⨯ ⁘
🦄

U  ▲ △ ⬆ ⇧ ◩ ◓ ◒
D  ▼ ▽ ⬇ ⇩ ◪ ◒ ◓
L  ◀ ◁ ⬅ ⇦ ⬕ ◐ ◑
R  ▶ ▷ ➡ ⇨ ⬔ ◑ ◐
```
