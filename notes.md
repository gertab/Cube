# Notes

moves notation:

- standard Singmaster notation,
- only the simple moves: R, L, ... x, y, x, (no M/S/E or double moves R2, L2,...)

functional implementations:

- face moves/rotation (simple to explain but not trivial & chatgpt struggles with it)
<!-- certain rotations require reversal of rows, e.g. R (from top to back) -->
- parser from string to cube/stringify from cube to string
- locating pieces (edges, corners, centers)
- rotate cube to a default orientation (e.g. white center at bottom)
- invert move sequence
- minimize move sequence
- is cube solved
- some solving specific functions: cross solved, first two layers solved, yellow face solved
- solve cube (using available intermediary solving steps)


<!--
open Rubiks_cube.Cube;; 
open Rubiks_cube.Util;; 
-->
