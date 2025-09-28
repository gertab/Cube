open OUnit2
open Rubiks_cube.Cube
open Rubiks_cube.Util
open Solver
open Common

let simple_moves = 
  [
  ("solved", []);
  ("twisted", [U]);
  ("twisted, further", [U; Y]);
  ("basic", [R'; U; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; U'; F]); (* simple *)
  ("basic + U", [R'; U; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; U'; F] @ [U]);
  ("basic + Y", [R'; U; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; U'; F] @ [Y]);
  ("basic + U2", [R'; U; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; U'; F] @ [U; U]);
  ("basic + Y2", [R'; U; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; U'; F] @ [Y; Y]);
  ("basic + Y U'", [R'; U; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; U'; F] @ [Y; U']);
]

let inverse_pll_moves = 
  [
    ("012345678", invert_moves ([]));
    ("810345672", invert_moves ([X; R'; U; R'] @ d2 @ [R; U'; R'] @ d2 @ [R; R; X']));
    ("018345276", invert_moves ([X'; R; U'; R] @ d2 @ [R'; U; R] @ d2 @ [R; R; X]));
    ("012743658", invert_moves ([R; R; U; R; U; R'; U'; R'; U'; R'; U; R']));
    ("012547638", invert_moves ([R; U'; R; U; R; U; R; U'; R'; U'; R; R]));
    ("072543618", invert_moves (m2 @ [U] @ m2 @ u2 @ m2 @ [U] @ m2));
    ("018543672", invert_moves ([R; U; R'; U'; R'; F; R; R; U'; R'; U'; R; U; R'; F']));
    ("230145678", invert_moves ([R'; U; L'; U; U; R; U'; R'; U; U; R; L; U']));
    ("018347652", invert_moves ([R; U; R'; F'; R; U; R'; U'; R'; F; R; R; U'; R'; U']));
    ("210745638", invert_moves ([L; U; U; L'; U; U; L; F'; L'; U'; L; U; L; F; L; L; U]));
    ("210347658", invert_moves ([R'; U; U; R; U; U; R'; F; R; U; R'; U'; R'; F'; R; R; U']));
    ("852341670", invert_moves ([R'; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; F]));
    ("650143278", invert_moves ([R; R; Y; D; R'; U; R'; U'; R; Y'; D'; R; R; Y'; R'; U; R]));
    ("832745016", invert_moves ([R'; U'; R; Y; R; R; Y; D; R'; U; R; U'; R; Y'; D'; R; R]));
    ("812743056", invert_moves ([R; R; Y'; D'; R; U'; R; U; R'; Y; D; R; R; Y; R; U'; R']));
    ("670145238", invert_moves ([R; U; R'; Y'; R; R; Y'; D'; R; U'; R'; U; R'; Y; D; R; R]));
    ("012543876", invert_moves ([R'; U; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; U'; F]));
    ("032147658", invert_moves (m2 @ [U] @ m2 @ [U] @ m' @ u2 @ m2 @ u2 @ m' @ u2));
    ("832145670", invert_moves ([F; R; U'; R'; U'; R; U; R'; F'; R; U; R'; U'; R'; F; R; F']));
    ("872345610", invert_moves ([L; U'; R; U; U; L'; U; R'; L; U'; R; U; U; L'; U; R'; U]));
    ("076345218", invert_moves ([R'; U; L'; U; U; R; U'; L; R'; U; L'; U; U; R; U'; L; U']));
    ("618345072", invert_moves ([X'; R; U'; R'; D; R; U; R'; D'; R; U; R'; D; R; U'; R'; D'; X]));
  ]


(* One test per OPLLL case: name is the pattern string, logic is "scramble -> solve_pll -> is_pll_solved" *)
let mk_test (name:string) (inv_ms:move list) : test =
  name >:: fun _ ->
    let c1 = apply_moves inv_ms solved_cube in
    let c1' = orient_cube_with_white_down c1 in
    let sol = Solver.solve_pll c1' in
    let c2 = apply_moves sol c1' in
    assert_bool "PLL should be solved" (is_solved c2)

(* inverse_pll_moves is your existing [(pattern, invert_moves...)] table *)
let tests : test list =
  List.map (function pat, inv_ms -> mk_test pat inv_ms) inverse_pll_moves @
  List.map (function pat, inv_ms -> mk_test pat inv_ms) simple_moves

let suite: test =
  "pll tests" >::: tests

let () = run_test_tt_main suite
