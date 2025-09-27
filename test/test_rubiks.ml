open OUnit2
open Rubiks_cube.Cube
open Rubiks_cube.Util
open Common

let trials = 100
let scramble_len = 40

let solve_all (c:cube) : move list =
  let m1 = Solver.Cross.solve_white_cross c in
  let c1 = apply_moves m1 c in
  let m2 = Solver.F2l.solve_f2l c1 in
  let c2 = apply_moves m2 c1 in
  let m3 = Solver.Oll.solve_oll c2 in
  let c3 = apply_moves m3 c2 in
  let m4 = Solver.Pll.solve_pll c3 in
  let sol = m1 @ m2 @ m3 @ m4 in
  minimize_moves sol

let mk_case i : test =
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
  List.init trials (fun i -> mk_case (i+1))

let suite : test =
  "random solve tests" >::: tests

let () = run_test_tt_main suite