open Rubiks.Rubikscube
open Rubiks

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

let _scrambled_cube = apply_moves _cube_1 [L; L; B; B; L'; F; F; D; B'; F';]

let () =
  Printf.printf "Start\n%s\n" (cube_of_string _scrambled_cube)


let moves = Solve.solve_white_cross _scrambled_cube

let crossed = apply_moves _scrambled_cube moves
let () = Printf.printf "Moves to solve white cross: %s\n"
  (String.concat " " (List.map string_of_move moves))

let () =
  Printf.printf "Final\n%s\n" (cube_of_string crossed)


(* crossed now has the white cross on Down, with side colours matched to their centers *)

(* given your apply_move / apply_moves *)
(* let cross_moves = Solve.solve_white_cross scrambled_cube *)

(* let () = Printf.printf "Moves to solve white cross: %s\n"
  (String.concat " " (List.map string_of_move cross_moves))
let crossed = apply_moves scrambled_cube cross_moves *)

(* pretty-print to check *)
(* let () =
  Printf.printf "%s\n" (cube_of_string (crossed)) *)


(* let () =
  Printf.printf "%s\n" (cube_of_string (apply_moves solved_cube [L; L; B; B; L'; F; F; D; B'; F'; D'; X; Y; Z; ]))
   *)


(* let () = Printf.printf "The answer is: %s\n" () *)