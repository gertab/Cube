open Rubiks_cube.Cube
open Rubiks_cube.Util

let u2 = [U; U]
let d2 = [D; D]
let m  = [X'; R; L']     (* M  = X' R L' *)
let m' = [L; R'; X]      (* M' = L  R' X *)
let m2 = m @ m           (* M2 *)

(* PLL orientation signature *)

type want =
  | WantCorner of face_label * face_label  (* side faces, Yellow implicit *)
  | WantEdge   of face_label               (* side face, Yellow implicit *)
  | WantCenter

let desired_slots : want list =
  [ WantCorner (B_face, L_face);  (* BLU -> UBL *)
    WantEdge   (B_face);          (* BU  -> UB  *)
    WantCorner (B_face, R_face);  (* BRU -> URB *)
    WantEdge   (L_face);          (* LU  -> UL  *)
    WantCenter;                   (* U-center   *)
    WantEdge   (R_face);          (* RU  -> UR  *)
    WantCorner (L_face, F_face);  (* FLU -> ULF *)
    WantEdge   (F_face);          (* FU  -> UF  *)
    WantCorner (F_face, R_face) ] (* FRU -> UFR *)

let actual_corners = [UBL; URB; ULF; UFR]
let actual_edges   = [UB; UL; UR; UF]

let index_of_actual_corner = function
  | UBL -> 0 | URB -> 2 | ULF -> 6 | UFR -> 8
  | _ -> failwith "not a U-layer corner"

let index_of_actual_edge = function
  | UB -> 1 | UL -> 3 | UR -> 5 | UF -> 7
  | _ -> failwith "not a U-layer edge"

let corner_has_colours (c:cube) (pos:corner_pos) (a:colour) (b:colour) : bool =
  let (x1,x2,x3) = corner_colours c pos in
  let set = List.sort compare [x1;x2;x3]
  and tgt = List.sort compare [a; b; Yellow] in
  set = tgt

let edge_has_colours (c:cube) (pos:edge_pos) (a:colour) : bool =
  let (x1,x2) = edge_colours c pos in
  (x1 = a && x2 = Yellow) || (x1 = Yellow && x2 = a)

let pll_signature (c0:cube) : int list =
  let c = orient_cube_with_white_down c0 in
  List.map
    (function
      | WantCorner (fA, fB) ->
          let a = center_of c fA and b = center_of c fB in
          let pos =
            match List.find_opt (fun p -> corner_has_colours c p a b) actual_corners with
            | Some p -> p
            | None -> failwith "PLL: desired U-corner colours not found"
          in
          index_of_actual_corner pos
      | WantEdge fA ->
          let a = center_of c fA in
          let pos =
            match List.find_opt (fun p -> edge_has_colours c p a) actual_edges with
            | Some p -> p
            | None -> failwith "PLL: desired U-edge colours not found"
          in
          index_of_actual_edge pos
      | WantCenter -> 4)
    desired_slots

let steps_pll (p:int list) : move list =
  match p with
  | [8;1;0;3;4;5;6;7;2] -> [X; R'; U; R'] @ d2 @ [R; U'; R'] @ d2 @ [R; R; X']
  | [0;1;8;3;4;5;2;7;6] -> [X'; R; U'; R] @ d2 @ [R'; U; R] @ d2 @ [R; R; X]
  | [0;1;2;7;4;3;6;5;8] -> [R; R; U; R; U; R'; U'; R'; U'; R'; U; R']
  | [0;1;2;5;4;7;6;3;8] -> [R; U'; R; U; R; U; R; U'; R'; U'; R; R]
  | [0;7;2;5;4;3;6;1;8] -> m2 @ [U] @ m2 @ u2 @ m2 @ [U] @ m2
  | [0;1;8;5;4;3;6;7;2] -> [R; U; R'; U'; R'; F; R; R; U'; R'; U'; R; U; R'; F']
  | [2;3;0;1;4;5;6;7;8] -> [R'; U; L'; U; U; R; U'; R'; U; U; R; L; U']
  | [0;1;8;3;4;7;6;5;2] -> [R; U; R'; F'; R; U; R'; U'; R'; F; R; R; U'; R'; U']
  | [2;1;0;7;4;5;6;3;8] -> [L; U; U; L'; U; U; L; F'; L'; U'; L; U; L; F; L; L; U]
  | [2;1;0;3;4;7;6;5;8] -> [R'; U; U; R; U; U; R'; F; R; U; R'; U'; R'; F'; R; R; U']
  | [8;5;2;3;4;1;6;7;0] -> [R'; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; F]
  | [6;5;0;1;4;3;2;7;8] -> [R; R; Y; D; R'; U; R'; U'; R; Y'; D'; R; R; Y'; R'; U; R]
  | [8;3;2;7;4;5;0;1;6] -> [R'; U'; R; Y; R; R; Y; D; R'; U; R; U'; R; Y'; D'; R; R]
  | [8;1;2;7;4;3;0;5;6] -> [R; R; Y'; D'; R; U'; R; U; R'; Y; D; R; R; Y; R; U'; R']
  | [6;7;0;1;4;5;2;3;8] -> [R; U; R'; Y'; R; R; Y'; D'; R; U'; R'; U; R'; Y; D; R; R]
(* [R'; U2; R'; Y; U'; R'; F'; R2; U'; R'; U; R'; F; R; U'; F], *)
  | [0;1;2;5;4;3;8;7;6] -> [R'; U; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; U'; F]
  | [0;3;2;1;4;7;6;5;8] -> m2 @ [U] @ m2 @ [U] @ m' @ u2 @ m2 @ u2 @ m' @ u2
  | [8;3;2;1;4;5;6;7;0] -> [F; R; U'; R'; U'; R; U; R'; F'; R; U; R'; U'; R'; F; R; F']
  | [8;7;2;3;4;5;6;1;0] -> [L; U'; R; U; U; L'; U; R'; L; U'; R; U; U; L'; U; R'; U]
  | [0;7;6;3;4;5;2;1;8] -> [R'; U; L'; U; U; R; U'; L; R'; U; L'; U; U; R; U'; L; U']
  | [6;1;8;3;4;5;0;7;2] -> [X'; R; U'; R'; D; R; U; R'; D'; R; U; R'; D; R; U'; R'; D'; X]
  | _ -> []

let solve_pll (c0:cube) : move list =
  let is_white_center_down = eq_colour (center_of c0 D_face) White in
  if not (Oll.is_oll_solved c0) then
    failwith "[pll] cube does not have OLL solved"
  else if not is_white_center_down then
    failwith "[pll] cube not oriented with white center on Down face"
  else if is_solved c0 then
    []  (* already solved *)
  else 
    let rec try_u c i =
      if i = 4 then
        (* steps_pll [0;7;2;5;4;3;6;1;8]  fallback *)
        failwith "[pll] no PLL case matched" (* check *)
      else
        let rec try_y c j =
          if j = 4 then
            try_u (apply_move U c) (i+1)
          else
            let sig_ = pll_signature c in
            match steps_pll sig_ with
            | [] -> try_y (apply_move Y c) (j+1)
            | ms -> 
              Printf.printf "signature: [%s]\n"
                (String.concat "; " (List.map string_of_int sig_));
              Printf.printf "cube as now:\n%s\n" (string_of_cube c);
              ms
        in
        try_y (orient_cube_with_white_down c) 0
    in
    try_u c0 0
    |> minimize_moves
