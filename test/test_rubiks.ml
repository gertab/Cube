open OUnit2
open Rubiks_cube.Cube
open Rubiks_cube.Util
open Solver

let scrambles = [
  [];
  [X];
  [Y];
  [Z];
  [R; U; R; U];
  [F'; R; F'; B; D'];
  [R'; R'; L'; Z; F];
  [X; L; B'; B; U];
  [R'; R'; D; B; U'];
  [B; Z; R; L; R'; F; D'; R; Y; F'];
  [U'; Z; F; L'; Y; F; Y; L; F'; R];
  [D; F; Y; L; D; B; F; R; D'; F'];
  [Y; R; B'; R'; F'; B'; F'; L'; Y; F'];
  [F; R'; F'; U; F'; F'; U; R; Z; X];
  [R'; R'; X; L'; B'; F'; L'; U'; F; D; L; R'; F'; Z; R'; F; U'; B; U; U];
  [R; F; B'; R; U; D; B; U; U'; U'; Y; F'; D; R; F'; U'; U'; F'; L; Z];
  [R; B'; L'; F'; D'; Z; R'; R'; B; Y; U'; R'; R; B'; Y; L; R'; D; U; U'];
  [L'; F'; R'; R'; B'; F; R; R; F; L; Z; F'; D; L; U'; X; U; B; R; D'];
  [F; U; L; B'; Z; R'; D'; X; B; L'; F'; U'; U'; F; F'; Z; R; U; R'; Z];
  [B'; F; L; D; L; Z; R; R'; R; R; Y; R'; D; U'; U'; L'; U'; R'; R'; B'];
  [R; U'; L; D; B'; R; U; F; F; L'; D; U; L'; F; L; D'; R'; D'; B'; U'];
  [F'; U'; R'; F; F; L; L'; B; R'; R; L; U; B'; B; Y; U'; Y; U; U'; F'];
  [R; U; R'; L'; R'; R; U'; R'; U'; U'; D; R; B; D'; B'; U; U'; R'; Y; R];
  [X; R'; D'; L; U'; Y; R; F; R; Z; R; F; L; U'; U; D'; U'; U'; B'; Z];
]

let scrambled_cubes = 
  [
    "YYYYYYYYYBBBBBBBBBRRRRRRRRRGGGGGGGGGOOOOOOOOOWWWWWWWWW";  (* solved *)
    "wowgybwyogygybyoggrowbrgywrborwggybrbwororbwborgowryby";
    "WOWGYBWYOGYGYBYOGGROWBRGYWRBORWGGYBRBWORORBWBORGOWRYBY";
    "owbroyyyogbrbwbbbggrwybgowoboroywyrwyoyrgwrgwwgborgryg";  (* orange top, blue front *)
  ]

let mk_case_scramble i : test =
  let scramble = List.nth scrambles (i-1) in
  let scramble_str = String.concat " " (List.map string_of_move scramble) in
  (Printf.sprintf "solve_%02d: %s" i scramble_str) >:: fun _ ->
    let c_scr = apply_moves scramble solved_cube in
    let sol = solve_all c_scr in
    let c_fin = apply_moves sol c_scr in
    if not (is_solved c_fin) then
      let sol_str      = String.concat " " (List.map string_of_move sol) in
      let cube_str     = string_of_cube c_fin in
      assert_failure
        (Printf.sprintf
           "Cube NOT solved!\nScramble: %s\nSolution: %s\nFinal cube:\n%s"
           scramble_str sol_str cube_str)


let mk_case_scramble_cube i : test =
  let cube_str = List.nth scrambled_cubes (i-1) in
  (Printf.sprintf "solve_%02d: %s" i cube_str) >:: fun _ ->
    let c = parse_cube_string cube_str in
    let sol = solve_all c in
    let c_fin = apply_moves sol c in
    if not (is_solved c_fin) then
      let sol_str      = String.concat " " (List.map string_of_move sol) in
      let cube_str     = string_of_cube c_fin in
      assert_failure
        (Printf.sprintf
           "Cube NOT solved!\nScramble: %s\nSolution: %s\nFinal cube:\n%s"
           cube_str sol_str cube_str)


let _trials = 100
let scramble_len = 40

let _mk_case_random i : test =
  (* random *)
  let scramble     = get_scramble scramble_len in
  let scramble_str = String.concat " " (List.map string_of_move scramble) in
  (Printf.sprintf "random_solve_%02d: %s" i scramble_str) >:: fun _ ->
    let c_scr = apply_moves scramble solved_cube in
    let sol = solve_all c_scr in
    let c_fin = apply_moves sol c_scr in
    if not (is_solved c_fin) then
      let sol_str      = String.concat " " (List.map string_of_move sol) in
      let cube_str     = string_of_cube c_fin in
      assert_failure
        (Printf.sprintf
           "Cube NOT solved!\nScramble: %s\nSolution: %s\nFinal cube:\n%s"
           scramble_str sol_str cube_str)


let tests : test list =
  (* List.init trials (fun i -> mk_case_random (i+1)) @@ *)
  List.init (List.length scrambled_cubes) (fun i -> mk_case_scramble (i+1)) @
  List.init (List.length scrambled_cubes) (fun i -> mk_case_scramble_cube (i+1))

let suite : test =
  "random solve tests" >::: tests

let () = run_test_tt_main suite