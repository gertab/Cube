open Rubiks_cube.Cube
open Rubiks_cube.Util

let () = print_endline "Hello, World!"

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



(* L L B B L' F F D B' F' *)
(* let scramble_moves = [F; U; F] *)
(* let scramble_moves = get_scramble 3 *)
let scramble_moves = [
F'; U'; B';
F'; U; F; U'; F; F; Y; Y; F'; U; F; U'; F; F; Y; Y;
U; U; L'; U'; L; R; U; U; R'; U'; R; U; U; R'; Y; U'; R; U; R'; U; R; U; R'; Y; U'; R; U'; R'; U'; F'; U; F; Y; U; U; R; U'; R'; Y'; U; R'; U; R; Y; Y;
Y; L; F; R'; F; R; F; F; L'
]

let _scrambled_cube = apply_moves scramble_moves _cube_1
(* let _scrambled_cube = apply_moves [L; L; B; B; L'; F; X; F;Y; D; B'; F';
(* white cross *)
(* Z; Z; Z; F; R; U'; R'; F'; U; U; F; F; Y; R; F'; U; F; U; U; U; F; F; Y; L; F; U'; F'; L'; U; F; F; Y; B; L; U'; L'; B'; F; F; Y *)
] _cube_1 *)


let () =
  (* Printf.printf "Start\n%s\n" (string_of_cube _scrambled_cube); *)
  Printf.printf "Scramble moves: %s\n"
    (String.concat " " (List.map string_of_move scramble_moves))


let moves = Solver.Cross.solve_white_cross _scrambled_cube

let crossed = apply_moves moves _scrambled_cube

let () = Printf.printf "Moves to solve white cross: %s\n"
  (String.concat " " (List.map string_of_move moves))
(* let () = Printf.printf "white cross solved\n%s\n" (string_of_cube crossed) *)


let moves_f2l = Solver.F2l.solve_f2l crossed
let f2l = apply_moves moves_f2l crossed
let () = Printf.printf "Moves to solve f2l: %s\n"
  (String.concat " " (List.map string_of_move moves_f2l))
(* let () = Printf.printf "F2L solved\n%s\n" (string_of_cube f2l) *)

let moves_oll = Solver.Oll.solve_oll f2l
let oll = apply_moves moves_oll f2l
let () = Printf.printf "Moves to solve oll: %s\n"
  (String.concat " " (List.map string_of_move moves_oll))

let moves_pll = Solver.Pll.solve_pll oll
let pll = apply_moves moves_pll oll
let () = Printf.printf "Moves to solve pll: %s\n"
  (String.concat " " (List.map string_of_move moves_pll))

let () = Printf.printf "Final\n%s\n" (string_of_cube pll)

let () = Printf.printf "Is cross solved? %b\n" (Solver.Cross.is_white_cross_solved pll)
let () = Printf.printf "Is F2L solved? %b\n" (Solver.F2l.is_f2l_solved pll)
let () = Printf.printf "Is OLL solved? %b\n" (Solver.Oll.is_oll_solved pll)
let () = Printf.printf "Is solved? %b\n" (is_solved pll)




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