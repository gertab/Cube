open Rubiks_cube.Cube
open Rubiks_cube.Util

let _cube_1 ={
  front = mk_face
    Red Red Red
    Red Red Red
    Red Red Red;
  back = mk_face
    Orange Orange Orange
    Orange Orange Orange
    Orange Orange Orange;
  left = mk_face
    Blue Blue Blue
    Blue Blue Blue
    Blue Blue Blue;
  right = mk_face
    Green Green Green
    Green Green Green
    Green Green Green;
  up = mk_face
    Yellow Yellow Yellow
    Yellow Yellow Yellow
    Yellow Yellow Yellow;
  down = mk_face
    White White White
    White White White
    White White White;
} 

(* let scrambled_cube = parse_cube_string "WOWGYBWYOGYGYBYOGGROWBRGYWRBORWGGYBRBWORORBWBORGOWRYBY"
let scramble_moves =  invert_moves (Solver.solve_all scrambled_cube)
let scrambled_cube = apply_moves scramble_moves solved_cube
let () = Printf.printf "Scrambled cube:\n%s\n" (string_of_cube scrambled_cube) *)

(* let scrambled_cube = parse_cube_string "wowgybwyogygybyoggrowbrgywrborwggybrbwororbwborgowryby"
let scramble_moves =  invert_moves (Solver.solve_all scrambled_cube)
let scrambled_cube = apply_moves scramble_moves solved_cube *)

(* let scramble_moves = [R; U; U; Y] *)
let scramble_moves = [L; L; B; B; L'; F; F; D; B'; F']
let scrambled_cube = apply_moves scramble_moves solved_cube
let () = Printf.printf "Scramble moves: %s\n" (String.concat " " (List.map string_of_move scramble_moves))

(* Cross *)
let moves = Solver.solve_white_cross scrambled_cube
let crossed = apply_moves moves scrambled_cube
let () = Printf.printf "Moves to solve white cross: %s\n"
  (String.concat " " (List.map string_of_move moves))

(* F2L *)
let moves_f2l = Solver.solve_f2l crossed
let f2l = apply_moves moves_f2l crossed
let () = Printf.printf "Moves to solve f2l: %s\n"
  (String.concat " " (List.map string_of_move moves_f2l))
(* let () = Printf.printf "F2L solved\n%s\n" (string_of_cube f2l) *)

(* OLL *)
let moves_oll = Solver.solve_oll f2l
let oll = apply_moves moves_oll f2l
let () = Printf.printf "Moves to solve oll: %s\n"
  (String.concat " " (List.map string_of_move moves_oll))

(* OLL *)
let moves_pll = Solver.solve_pll oll
let pll = apply_moves moves_pll oll
let () = Printf.printf "Moves to solve pll: %s\n"
  (String.concat " " (List.map string_of_move moves_pll))

let () = Printf.printf "Final\n%s\n" (string_of_cube pll)

let () = 
  Printf.printf "Is cross solved? %b\n" (Solver.is_white_cross_solved pll); 
  Printf.printf "Is F2L solved? %b\n" (Solver.is_f2l_solved pll);
  Printf.printf "Is OLL solved? %b\n" (Solver.is_oll_solved pll);
  Printf.printf "Is solved? %b\n" (Solver.is_solved pll);
  Printf.printf "Visualise: %s\n"
    (twizzle_url 
       ~scramble:scramble_moves
       ~cross:moves
       ~f2l:moves_f2l
       ~oll:moves_oll
       ~pll:moves_pll
    )



(* visualise solution in https://alpha.twizzle.net/ *)



(* crossed now has the white cross on Down, with side colours matched to their centers *)

(* given your apply_move / apply_moves *)
(* let cross_moves = Solve.solve_white_cross scrambled_cube *)

(* let () = Printf.printf "Moves to solve white cross: %s\n"
  (String.concat " " (List.map string_of_move cross_moves))
let crossed = apply_moves scrambled_cube cross_moves *)

(* pretty-print to check *)
(* let () =
  Printf.printf "%s\n" (string_of_cube (crossed)) *)


(* let () =
  Printf.printf "%s\n" (string_of_cube (apply_moves solved_cube [L; L; B; B; L'; F; F; D; B'; F'; D'; X; Y; Z; ]))
   *)


(* let () = Printf.printf "The answer is: %s\n" () *)