open Rubiks_cube.Cube
open Rubiks_cube.Util

(* CFOP: solving the PLL (Permutation of the Last Layer)

   - assumes OLL is already solved (all stickers on the U face are yellow)
   - PLL permutes last-layer corners and edges to complete the cube
   - implemented with a pattern -> algorithm lookup table (21 PLL cases)
   - pattern is represented as a list of integers [0..8] describing the U-layer cubies’ permutation
   - search strategy:
       * orient cube with white center on D
       * spin U and Y to align with a known pattern (try up to 4 U and 4 Y)
       * apply the corresponding PLL algorithm
       * include the U/Y rotations used during search in the final move list
*)

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

let steps_pll (p:int list) : move list * bool =
  match p with
  | [0;1;2;3;4;5;6;7;8] -> [], true
  | [8;1;0;3;4;5;6;7;2] -> [X; R'; U; R'] @ d2 @ [R; U'; R'] @ d2 @ [R; R; X'], true
  | [0;1;8;3;4;5;2;7;6] -> [X'; R; U'; R] @ d2 @ [R'; U; R] @ d2 @ [R; R; X], true
  | [0;1;2;7;4;3;6;5;8] -> [R; R; U; R; U; R'; U'; R'; U'; R'; U; R'], true
  | [0;1;2;5;4;7;6;3;8] -> [R; U'; R; U; R; U; R; U'; R'; U'; R; R], true
  | [0;7;2;5;4;3;6;1;8] -> m2 @ [U] @ m2 @ u2 @ m2 @ [U] @ m2, true
  | [0;1;8;5;4;3;6;7;2] -> [R; U; R'; U'; R'; F; R; R; U'; R'; U'; R; U; R'; F'], true
  | [2;3;0;1;4;5;6;7;8] -> [R'; U; L'; U; U; R; U'; R'; U; U; R; L; U'], true
  | [0;1;8;3;4;7;6;5;2] -> [R; U; R'; F'; R; U; R'; U'; R'; F; R; R; U'; R'; U'], true
  | [2;1;0;7;4;5;6;3;8] -> [L; U; U; L'; U; U; L; F'; L'; U'; L; U; L; F; L; L; U], true
  | [2;1;0;3;4;7;6;5;8] -> [R'; U; U; R; U; U; R'; F; R; U; R'; U'; R'; F'; R; R; U'], true
  | [8;5;2;3;4;1;6;7;0] -> [R'; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; F], true
  | [6;5;0;1;4;3;2;7;8] -> [R; R; Y; D; R'; U; R'; U'; R; Y'; D'; R; R; Y'; R'; U; R], true
  | [8;3;2;7;4;5;0;1;6] -> [R'; U'; R; Y; R; R; Y; D; R'; U; R; U'; R; Y'; D'; R; R], true
  | [8;1;2;7;4;3;0;5;6] -> [R; R; Y'; D'; R; U'; R; U; R'; Y; D; R; R; Y; R; U'; R'], true
  | [6;7;0;1;4;5;2;3;8] -> [R; U; R'; Y'; R; R; Y'; D'; R; U'; R'; U; R'; Y; D; R; R], true
  | [0;1;2;5;4;3;8;7;6] -> [R'; U; U; R'; Y; U'; R'; F'; R; R; U'; R'; U; R'; F; R; U'; F], true
  | [0;3;2;1;4;7;6;5;8] -> m2 @ [U] @ m2 @ [U] @ m' @ u2 @ m2 @ u2 @ m' @ u2, true
  | [8;3;2;1;4;5;6;7;0] -> [F; R; U'; R'; U'; R; U; R'; F'; R; U; R'; U'; R'; F; R; F'], true
  | [8;7;2;3;4;5;6;1;0] -> [L; U'; R; U; U; L'; U; R'; L; U'; R; U; U; L'; U; R'; U], true
  | [0;7;6;3;4;5;2;1;8] -> [R'; U; L'; U; U; R; U'; L; R'; U; L'; U; U; R; U'; L; U'], true
  | [6;1;8;3;4;5;0;7;2] -> [X'; R; U'; R'; D; R; U; R'; D'; R; U; R'; D; R; U'; R'; D'; X], true
  | _ -> [], false (* not found *)

let solve_pll (c0:cube) : move list =
  let is_white_center_down = eq_colour (center_of c0 D_face) White in
  if not (Oll.is_oll_solved c0) then
    failwith "[pll] cube does not have OLL solved"
  else if not is_white_center_down then
    failwith "[pll] cube not oriented with white center on Down face"
  else if is_solved c0 then
    []  (* already solved *)
  else 
    let rec loop_u c acc_u i =
      if i = 4 then
        failwith "[pll] could not solve PLL: no matching algorithm"
      else
        let rec loop_y c acc_y j =
          if j = 4 then
            (* advance U, carry all Y spins done so far *)
            let c' = apply_move U c in
            loop_u c' (acc_u @ acc_y @ [U]) (i+1)
          else
            let sig_ = pll_signature c in
            match steps_pll sig_ with
            | _, false ->
                (* try another Y, accumulate it *)
                let c' = apply_move Y c in
                loop_y c' (acc_y @ [Y]) (j+1)
            | ms, true ->
                (* include the rotations we applied to reach this orientation *)
                acc_u @ acc_y @ ms
        in
        loop_y c [] 0
    in
    loop_u c0 [] 0
    |> minimize_moves
