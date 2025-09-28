open Rubiks_cube.Cube
open Rubiks_cube.Util

let solve_white_cross (c:cube) : move list = 
  Cross.solve_white_cross c

let is_white_cross_solved (c:cube) : bool = 
  Cross.is_white_cross_solved c

let solve_f2l (c:cube) : move list = 
  F2l.solve_f2l c

let is_f2l_solved (c:cube) : bool = 
  F2l.is_f2l_solved c

let solve_oll (c:cube) : move list = 
  Oll.solve_oll c

let is_oll_solved (c:cube) : bool = 
  Oll.is_oll_solved c

let solve_pll (c:cube) : move list = 
  Pll.solve_pll c

let solve_all (c:cube) : move list =
  let m1 = Cross.solve_white_cross c in
  let c1 = apply_moves m1 c in
  let m2 = F2l.solve_f2l c1 in
  let c2 = apply_moves m2 c1 in
  let m3 = Oll.solve_oll c2 in
  let c3 = apply_moves m3 c2 in
  let m4 = Pll.solve_pll c3 in
  let sol = m1 @ m2 @ m3 @ m4 in
  minimize_moves sol

let is_solved (c:cube) : bool =
  List.for_all (fun f ->
    let center = center_of c f in
    List.for_all ((=) center) (stickers_of_face c f)
  ) all_faces

