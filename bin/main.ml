open Rubiks_cube.Cube
open Rubiks_cube.Util

(* Start from a scrambled cube *)
(*          ----------
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
            ----------  *)
let scrambled_cube = parse_cube_string "wowgybwyogygybyoggrowbrgywrborwggybrbwororbwborgowryby"
let scramble_moves = invert_moves (Solver.solve_all scrambled_cube)


(* Alternative: provide the scramble moves *)
let _scramble_moves = [L; L; B; B; L'; F; F; D; B'; F']
let _scrambled_cube_2 = apply_moves _scramble_moves solved_cube
let () = Printf.printf "Scramble moves: %s\n" (String.concat " " (List.map string_of_move scramble_moves))


(* Solve the Cross *)
let moves = Solver.solve_white_cross scrambled_cube
let crossed = apply_moves moves scrambled_cube
let () = Printf.printf "Moves to solve white cross: %s\n"
  (String.concat " " (List.map string_of_move moves))

(* Solve the First 2 Layers *)
let moves_f2l = Solver.solve_f2l crossed
let f2l = apply_moves moves_f2l crossed
let () = Printf.printf "Moves to solve f2l: %s\n"
  (String.concat " " (List.map string_of_move moves_f2l))
(* let () = Printf.printf "F2L solved\n%s\n" (string_of_cube f2l) *)

(* Orient the Last Layer (OLL) *)
let moves_oll = Solver.solve_oll f2l
let oll = apply_moves moves_oll f2l
let () = Printf.printf "Moves to solve oll: %s\n"
  (String.concat " " (List.map string_of_move moves_oll))

(* Permutate the Last Layer (PLL) *)
let moves_pll = Solver.solve_pll oll
let pll = apply_moves moves_pll oll
let () = Printf.printf "Moves to solve pll: %s\n"
  (String.concat " " (List.map string_of_move moves_pll))

let () = Printf.printf "Final\n%s\n" (string_of_cube pll)

let () = 
  Printf.printf "Is cross solved? %b\n" (Solver.is_white_cross_solved pll); 
  Printf.printf "Is F2L solved? %b\n" (Solver.is_f2l_solved pll);
  Printf.printf "Is OLL solved? %b\n" (Solver.is_oll_solved pll);
  Printf.printf "Is cube solved? %b\n" (Solver.is_solved pll);
  Printf.printf "Visualise: %s\n"
    (visualise 
       ~scramble:scramble_moves
       ~cross:moves
       ~f2l:moves_f2l
       ~oll:moves_oll
       ~pll:moves_pll
    )


(* visualise solution in https://alpha.twizzle.net/ *)