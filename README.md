# Cube

A Rubik’s Cube simulator and solver implemented natively in OCaml.

The solver is inspired by this [project](https://github.com/Wiston999/python) and follows the [CFOP solving method](https://jperm.net/3x3/cfop), which consists of four main stages:

 1. Cross
 2. First Two Layers (F2L)
 3. Orientation of the Last Layer (OLL)
 4. Permutation of the Last Layer (PLL)

<!-- https://github.com/Wiston999/python-rubik/tree/master (uses algorithms from https://ruwix.com/the-rubiks-cube/advanced-cfop-fridrich/) -->

To run a built-in example:

```shell
dune exec bin/main.exe
```

Alternatively, use `make run`. To install any missing OCaml-related dependencies (e.g. dune), execute `make install`.

## Defining a Scrambled Cube

See [`bin/main.ml`](bin/main.ml) for an example of how to define and solve a scrambled cube.
You can create a scrambled cube in one of two ways:

### 1. From a List of Moves

``` ocaml
let scramble_moves = [L; L; B; B; L'; F; F; D; B'; F']
let scrambled_cube = apply_moves scramble_moves solved_cube
```

### 2. From a String Representation

```ocaml
let scrambled_cube = parse_cube_string "wowgybwyogygybyoggrowbrgywrborwggybrbwororbwborgowryby"
```
<!-- let scramble_moves = invert_moves (Solver.solve_all scrambled_cube) -->

The cube string follows this facelet order:

```text
            ----------
           |  1  2  3 |
           |  4  5  6 |
           |  7  8  9 |
 ---------- ---------- ---------- ----------
| 10 11 12 | 19 20 21 | 28 29 30 | 37 38 39 |
| 13 14 15 | 22 23 25 | 31 32 33 | 40 41 42 |
| 16 17 18 | 25 26 27 | 34 35 36 | 43 44 45 |
 ---------- ---------- ---------- ----------
           | 46 47 48 |
           | 49 50 51 |
           | 52 53 54 |
            ----------  
```

Cube moves (e.g. `L`, `U`, `R'`) and helper functions like `apply_moves` are defined in the `Cube` module, while utility functions (e.g. parsing, visualisation) are in the Util module:

```ocaml
open Rubiks_cube.Cube
open Rubiks_cube.Util
```

## Solving the Cube

The simplest way to solve a scrambled cube is with:

```ocaml
let moves = Solver.solve_all scrambled_cube
let solved_cube = apply_moves moves scrambled_cube
```

Alternatively, you can solve it step by step:

### Step 1: White Cross

```ocaml
let moves = Solver.solve_white_cross scrambled_cube
let crossed = apply_moves moves scrambled_cube
```

### Step 2: First Two Layers (F2L)

```ocaml
let moves_f2l = Solver.solve_f2l crossed
let f2l = apply_moves moves_f2l crossed
```

### Step 3: Orientation of the Last Layer (OLL)

```ocaml
let moves_oll = Solver.solve_oll f2l
let oll = apply_moves moves_oll f2l
```

### Step 4: Permutation of the Last Layer (PLL)

```ocaml
let moves_pll = Solver.solve_pll oll
let pll = apply_moves moves_pll oll
```

## Visualising the Solve

You can visualise the entire solution sequence using the `Util.visualise` function:

```ocaml
visualise 
    ~scramble:scramble_moves
    ~cross:moves
    ~f2l:moves_f2l
    ~oll:moves_oll
    ~pll:moves_pll
```

This generates a Twizzle-compatible URL for interactive 3D visualisation.
For example, the output for the sample solve is available here:
[alpha.twizzle.net/edit/?alg=R%27+R%27+U...](https://alpha.twizzle.net/edit/?alg=R%27+R%27+U+R+U+R%27+U%27+R%27+U%27+R%27+U+R%27+y%27+y%27+U+L+U+L%27+y%27+R%27+F+R+U%27+R%27+F%27+R+y%27+y%27+R+U%27+R%27+U%27+R+U%27+R%27+U%27+y%27+F%27+U%27+U%27+F+U%27+F%27+U%27+U%27+F+U%27+B+U%27+B%27+y%27+y%27+R%27+U+R+U%27+R%27+U+R+U%27+U%27+R%27+U+R+y+U%27+y%27+F%27+U%27+U%27+F+U%27+F%27+U%27+F+L%27+U%27+L+y%27+F%27+F%27+U%27+L+F+U+F%27+L%27+y%27+F%27+F%27+U+R%27+F%27+U%27+F+R+y%27+F%27+F%27+U%27+B%27+U%27+B+y%27+F%27+F%27+U%27+U%27+R+U+R%27+F+%2F%2F+scramble%0A%0A%2F%2F+solution%0AF%27+R+U%27+R%27+U+U+F+F+y+B%27+U+B+U+F+F+y+R%27+F%27+U+F+R+U%27+F+F+y+L+F+U%27+F%27+L%27+U+F+F+y+%2F%2F+white+cross%0AL%27+U+L+F%27+U+F+U+F%27+U+U+F+y+U+y%27+R%27+U%27+R+U+U+R%27+U%27+R+U+R%27+U%27+R+y+y+B+U+B%27+U+F%27+U+U+F+U+F%27+U+U+F+y+U+R+U+R%27+U+R+U+R%27+y+%2F%2F+f2l%0Ay+R%27+F+R+U+R%27+F%27+R+y+L+U%27+L%27+%2F%2F+oll%0AU%27+y+y+R+U%27+R+U+R+U+R+U%27+R%27+U%27+R+R+%2F%2F+pll&setup-alg=x+x+y)
