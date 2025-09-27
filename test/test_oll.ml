open OUnit2
open Rubiks_cube.Cube
open Rubiks_cube.Util
open Common

let m  = [X'; R; L']   (* M  *)
let m' = [L; R'; X]    (* M' *)
let e' = [U'; D; Y]    (* E' *)

let inverse_oll_moves = 
  [
    ("LBRLURLFR", inverse_moves ([R; U; B'; X'; R; U; X; X; R; R; X'; U'; R'; F; R; F']));
    ("LBRLURFFF", inverse_moves ([R'; F; R; F'; U; U; R'; F; R; Y'; R; R; U; U; R]));
    ("BBRLURLFU", inverse_moves ([Y] @ m @ [U; X; R'; U; U; X'; R; U; L'; U; L] @ m'));
    ("LBULURFFR", inverse_moves ([R'; U; U; X; R'; U; R; U'; Y; R'; U'; R'; U; R'; F; Z']));
    ("UBBLURLFU", inverse_moves ([R; U; R'; U; R'; F; R; F'; U; U; R'; F; R; F']));
    ("UBULURUFU", inverse_moves (m' @ [U; U] @ m @ [U; U] @ m' @ [U] @ m @ [U; U] @ m' @ [U; U] @ m));
    ("UBULURLFR", inverse_moves ([R'; U; U; F; R; U; R'; U'; Y'; R; R; U; U; X'; R; U; X]));
    ("BBBLURUFU", inverse_moves ([F; R; U; R'; U; Y'; R'; U; U; R'; F; R; F']));
    ("BURLURFUR", inverse_moves ([R'; U'; Y; L'; U; L'; Y'; L; F; L'; F; R]));
    ("LURLURLUR", inverse_moves ([R; U'; Y; R; R; D; R'; U; U; R; D'; R; R; Y'; U; R']));
    ("BBRUUUFFR", inverse_moves ([F; U; R; U'; R'; U; R; U'; R'; F']));
    ("LBRUUULFR", inverse_moves ([L'; B'; L; U'; R'; U; R; U'; R'; U; R; L'; B; L]));
    ("BURUUUFUR", inverse_moves ([L; U'; R'; U; L'; U; R; U; R'; U; R]));
    ("LURUUULUR", inverse_moves ([R; U; R'; U; R; U'; R'; U; R; U; U; R']));
    ("LUBUUUFUU", inverse_moves ([L'; U; R; U'; L; U; R']));
    ("BURUUULUU", inverse_moves ([R'; U; U; R; U; R'; U; R]));
    ("UUBUUUUUF", inverse_moves ([R'; F'; L; F; R; F'; L'; F]));
    ("UUUUUUFUF", inverse_moves ([R; R; D; R'; U; U; R; D'; R'; U; U; R']));
    ("UUBUUULUU", inverse_moves ([R'; F'; L'; F; R; F'; L; F]));
    ("UBUUURUUU", inverse_moves (m' @ [U'] @ m @ [U; U] @ m' @ [U'] @ m));
    ("UBUUUUUFU", inverse_moves ([L'; R; U; R'; U'; L; R'; F; R; F']));
    ("BURUURUFF", inverse_moves ([L; F; R'; F; R; F; F; L']));
    ("UURUURFFU", inverse_moves ([F; R'; F'; R; U; R; U'; R']));
    ("LUBUURFFU", inverse_moves ([R'; U'; R; Y'; X'; R; U'; R'; F; R; U; R'; X]));
    ("BUBUURUFU", inverse_moves ([U'; R; U; U; R'; U'; R; U'; R; R; Y'; R'; U'; R; U; B]));
    ("LUBUURLFF", inverse_moves ([F; R; U; R'; U'; R; U; R'; U'; F']));
    ("BUBUURFFF", inverse_moves ([L; F'; L'; F; U; U; L; L; Y'; L; F; L'; F]));
    ("BUBLUUUFU", inverse_moves ([U'; R'; U; U; R; U; R'; U; R; R; Y; R; U; R'; U'; F']));
    ("LUULUUFFR", inverse_moves ([X; L; U; U; R'; U'; R; U'; X'; L']));
    ("BUULUUUFR", inverse_moves ([R'; U; U; X'; R; R; U'; R'; U; X; R'; U; U; R]));
    ("BURLUUFFR", inverse_moves ([F'; L'; U'; L; U; L'; U'; L; U; F]));
    ("LUBLUULFF", inverse_moves ([R'; F; R'; F'; R; R; U; U; X'; U'; R; U; R'; X]));
    ("BUBLUUFFF", inverse_moves ([R'; F; R; F'; U; U; R; R; Y; R'; F'; R; F']));
    ("BBUUURLUF", inverse_moves ([R; U; R'; Y; R'; F; R; U'; R'; F'; R]));
    ("UBBUURFUR", inverse_moves ([L'; B'; L; U'; R'; U; R; L'; B; L]));
    ("LBBUURFUU", inverse_moves ([U; U; X; L; R; R; U'; R; U'; R'; U; U; R; U'] @ m));
    ("UBUUURLUR", inverse_moves ([X'; U'; R; U'; R; R; F; X; R; U; R'; U'; R; B; B]));
    ("LBBLUULUF", inverse_moves ([L; U'; Y'; R'; U; U; R'; U; R; U'; R; U; U; R; Y; U'; L']));
    ("BBRLUUUUF", inverse_moves ([U; U; X; R'; L; L; U; L'; U; L; U; U; L'; U] @ m));
    ("UBULUULUR", inverse_moves ([Y; Y; F; U; R; U'; X'; U; R'; D'; R; U'; R'; X]));
    ("BBRLUULUU", inverse_moves ([X'; L'; U; U; R; U; R'; U; X; L]));
    ("UURLURUUR", inverse_moves ([R; U; X'; R; U'; R'; U; X; U'; R']));
    ("LBRUUUUFU", inverse_moves ([R; U; R'; U'; X; D'; R'; U; R] @ e' @ [Z']));
    ("LBBUUUFFU", inverse_moves ([R'; F; R; U; R'; F'; R; Y; L; U'; L']));
    ("BBRUUUUFF", inverse_moves ([L; F'; L'; U'; L; F; L'; Y'; R'; U; R]));
    ("BBRUUULFU", inverse_moves ([L'; B'; L; R'; U'; R; U; L'; B; L]));
    ("LBBUUUUFR", inverse_moves ([R; B; R'; L; U; L'; U'; R; B'; R']));
    ("UURUURUFR", inverse_moves ([F; U; R; U'; R'; F']));
    ("BUULUUFFU", inverse_moves ([R'; Y; U'; L; Y'; U; R; U'; R'; F'; R]));
    ("UUBUURUFF", inverse_moves ([L; Y'; U; R'; Y; U'; L'; U; L; F; L']));
    ("LUULUULFU", inverse_moves ([F'; U'; L'; U; L; F]));
    ("LBUUUULFU", inverse_moves ([F; R; U; R'; U'; F']));
    ("BBUUUUFFU", inverse_moves ([R; U; R'; U'; R'; F; R; F']));
    ("LBULUUUUF", inverse_moves ([L; U; L'; U; L; U'; L'; U'; Y; Y; R'; F; R; F']));
    ("UBRUURFUU", inverse_moves ([R'; U'; R; U'; R'; U; R; U; Y; F; R'; F'; R]));
    ("UBBUUULFU", inverse_moves ([R'; F; R; U; R'; U'; Y; L'; Y'; U; R]));
    ("BBUUUUUFR", inverse_moves ([L; F'; L'; U'; L; U; Y'; R; Y; U'; L']))
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

(* inverse_oll_moves is your existing [(pattern, inverse_moves...)] table *)
let tests : test list =
  List.map
    (function pat, inv_ms -> mk_test pat inv_ms)
    inverse_oll_moves 

let suite: test =
  "oll tests" >::: tests

let () = run_test_tt_main suite
