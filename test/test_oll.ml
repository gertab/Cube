open OUnit2
open Rubiks_cube.Cube
open Rubiks_cube.Util
open Common

let m  = [X'; R; L']   (* M  *)
let m' = [L; R'; X]    (* M' *)
let e' = [U'; D; Y]    (* E' *)

let inverse_oll_moves = 
  [
    ("LBRLURLFR", invert_moves ([R; U; B'; X'; R; U; X; X; R; R; X'; U'; R'; F; R; F']));
    ("LBRLURFFF", invert_moves ([R'; F; R; F'; U; U; R'; F; R; Y'; R; R; U; U; R]));
    ("BBRLURLFU", invert_moves ([Y] @ m @ [U; X; R'; U; U; X'; R; U; L'; U; L] @ m'));
    ("LBULURFFR", invert_moves ([R'; U; U; X; R'; U; R; U'; Y; R'; U'; R'; U; R'; F; Z']));
    ("UBBLURLFU", invert_moves ([R; U; R'; U; R'; F; R; F'; U; U; R'; F; R; F']));
    ("UBULURUFU", invert_moves (m' @ [U; U] @ m @ [U; U] @ m' @ [U] @ m @ [U; U] @ m' @ [U; U] @ m));
    ("UBULURLFR", invert_moves ([R'; U; U; F; R; U; R'; U'; Y'; R; R; U; U; X'; R; U; X]));
    ("BBBLURUFU", invert_moves ([F; R; U; R'; U; Y'; R'; U; U; R'; F; R; F']));
    ("BURLURFUR", invert_moves ([R'; U'; Y; L'; U; L'; Y'; L; F; L'; F; R]));
    ("LURLURLUR", invert_moves ([R; U'; Y; R; R; D; R'; U; U; R; D'; R; R; Y'; U; R']));
    ("BBRUUUFFR", invert_moves ([F; U; R; U'; R'; U; R; U'; R'; F']));
    ("LBRUUULFR", invert_moves ([L'; B'; L; U'; R'; U; R; U'; R'; U; R; L'; B; L]));
    ("BURUUUFUR", invert_moves ([L; U'; R'; U; L'; U; R; U; R'; U; R]));
    ("LURUUULUR", invert_moves ([R; U; R'; U; R; U'; R'; U; R; U; U; R']));
    ("LUBUUUFUU", invert_moves ([L'; U; R; U'; L; U; R']));
    ("BURUUULUU", invert_moves ([R'; U; U; R; U; R'; U; R]));
    ("UUBUUUUUF", invert_moves ([R'; F'; L; F; R; F'; L'; F]));
    ("UUUUUUFUF", invert_moves ([R; R; D; R'; U; U; R; D'; R'; U; U; R']));
    ("UUBUUULUU", invert_moves ([R'; F'; L'; F; R; F'; L; F]));
    ("UBUUURUUU", invert_moves (m' @ [U'] @ m @ [U; U] @ m' @ [U'] @ m));
    ("UBUUUUUFU", invert_moves ([L'; R; U; R'; U'; L; R'; F; R; F']));
    ("BURUURUFF", invert_moves ([L; F; R'; F; R; F; F; L']));
    ("UURUURFFU", invert_moves ([F; R'; F'; R; U; R; U'; R']));
    ("LUBUURFFU", invert_moves ([R'; U'; R; Y'; X'; R; U'; R'; F; R; U; R'; X]));
    ("BUBUURUFU", invert_moves ([U'; R; U; U; R'; U'; R; U'; R; R; Y'; R'; U'; R; U; B]));
    ("LUBUURLFF", invert_moves ([F; R; U; R'; U'; R; U; R'; U'; F']));
    ("BUBUURFFF", invert_moves ([L; F'; L'; F; U; U; L; L; Y'; L; F; L'; F]));
    ("BUBLUUUFU", invert_moves ([U'; R'; U; U; R; U; R'; U; R; R; Y; R; U; R'; U'; F']));
    ("LUULUUFFR", invert_moves ([X; L; U; U; R'; U'; R; U'; X'; L']));
    ("BUULUUUFR", invert_moves ([R'; U; U; X'; R; R; U'; R'; U; X; R'; U; U; R]));
    ("BURLUUFFR", invert_moves ([F'; L'; U'; L; U; L'; U'; L; U; F]));
    ("LUBLUULFF", invert_moves ([R'; F; R'; F'; R; R; U; U; X'; U'; R; U; R'; X]));
    ("BUBLUUFFF", invert_moves ([R'; F; R; F'; U; U; R; R; Y; R'; F'; R; F']));
    ("BBUUURLUF", invert_moves ([R; U; R'; Y; R'; F; R; U'; R'; F'; R]));
    ("UBBUURFUR", invert_moves ([L'; B'; L; U'; R'; U; R; L'; B; L]));
    ("LBBUURFUU", invert_moves ([U; U; X; L; R; R; U'; R; U'; R'; U; U; R; U'] @ m));
    ("UBUUURLUR", invert_moves ([X'; U'; R; U'; R; R; F; X; R; U; R'; U'; R; B; B]));
    ("LBBLUULUF", invert_moves ([L; U'; Y'; R'; U; U; R'; U; R; U'; R; U; U; R; Y; U'; L']));
    ("BBRLUUUUF", invert_moves ([U; U; X; R'; L; L; U; L'; U; L; U; U; L'; U] @ m));
    ("UBULUULUR", invert_moves ([Y; Y; F; U; R; U'; X'; U; R'; D'; R; U'; R'; X]));
    ("BBRLUULUU", invert_moves ([X'; L'; U; U; R; U; R'; U; X; L]));
    ("UURLURUUR", invert_moves ([R; U; X'; R; U'; R'; U; X; U'; R']));
    ("LBRUUUUFU", invert_moves ([R; U; R'; U'; X; D'; R'; U; R] @ e' @ [Z']));
    ("LBBUUUFFU", invert_moves ([R'; F; R; U; R'; F'; R; Y; L; U'; L']));
    ("BBRUUUUFF", invert_moves ([L; F'; L'; U'; L; F; L'; Y'; R'; U; R]));
    ("BBRUUULFU", invert_moves ([L'; B'; L; R'; U'; R; U; L'; B; L]));
    ("LBBUUUUFR", invert_moves ([R; B; R'; L; U; L'; U'; R; B'; R']));
    ("UURUURUFR", invert_moves ([F; U; R; U'; R'; F']));
    ("BUULUUFFU", invert_moves ([R'; Y; U'; L; Y'; U; R; U'; R'; F'; R]));
    ("UUBUURUFF", invert_moves ([L; Y'; U; R'; Y; U'; L'; U; L; F; L']));
    ("LUULUULFU", invert_moves ([F'; U'; L'; U; L; F]));
    ("LBUUUULFU", invert_moves ([F; R; U; R'; U'; F']));
    ("BBUUUUFFU", invert_moves ([R; U; R'; U'; R'; F; R; F']));
    ("LBULUUUUF", invert_moves ([L; U; L'; U; L; U'; L'; U'; Y; Y; R'; F; R; F']));
    ("UBRUURFUU", invert_moves ([R'; U'; R; U'; R'; U; R; U; Y; F; R'; F'; R]));
    ("UBBUUULFU", invert_moves ([R'; F; R; U; R'; U'; Y; L'; Y'; U; R]));
    ("BBUUUUUFR", invert_moves ([L; F'; L'; U'; L; U; Y'; R; Y; U'; L']))
  ]

(* One test per OLL case: name is the pattern string, logic is "scramble -> solve_oll -> is_oll_solved" *)
let mk_test (name:string) (inv_ms:move list) : test =
  name >:: fun _ ->
    let c1 = apply_moves inv_ms solved_cube in
    let c1' = orient_cube_with_white_down c1 in
    (* let c1'' = Solver.F2l.solve_f2l c1' in *)
    let sol = Solver.Oll.solve_oll c1' in
    let c2 = apply_moves sol c1' in
    assert_bool "OLL should be solved" (Solver.Oll.is_oll_solved c2)

(* inverse_oll_moves is your existing [(pattern, invert_moves...)] table *)
let tests : test list =
  List.map
    (function pat, inv_ms -> mk_test pat inv_ms)
    inverse_oll_moves 

let suite: test =
  "oll tests" >::: tests

let () = run_test_tt_main suite
