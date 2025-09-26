open Rubiks_cube.Cube
open Rubiks_cube.Util

(* CFOP: solving the white cross

   - cube has to be oriented with the white center on the Down face
   - solves the white cross on the Down face, with side colours matching center pieces
   - there are four white edge pieces to place
   - step by step:
     1. locate a white edge piece the matches the color of the front center
     2. bring it to the Up face with white facing up
     3. spin U to get it on the UF position
     4. do F2 to insert it into the Down face
     5. do a Y turn to prepare for the next edge
*)

(* Step 2: *)
(* White-cross step table (white-facing × other-facing -> moves) *)
let steps (white:face_label) (other:face_label) : move list =
  match white, other with
  (* white already facing U: nothing to put it on U *)
  | U_face, R_face | U_face, L_face | U_face, F_face | U_face, B_face -> []

  (* white on D: flip up with a double turn on the other face *)
  | D_face, R_face -> [R; R]
  | D_face, L_face -> [L; L]
  | D_face, F_face -> [F; F]
  | D_face, B_face -> [B; B]

  (* white on a side: sequences to bring it to U *)
  | F_face, U_face -> [F; R; U'; R'; F']
  | F_face, D_face -> [F'; R; U'; R']
  | F_face, R_face -> [R; U; R']
  | F_face, L_face -> [L'; U'; L]

  | B_face, U_face -> [B; L; U'; L'; B']
  | B_face, D_face -> [B; R'; U; R]
  | B_face, R_face -> [R'; U; R]
  | B_face, L_face -> [L; U'; L']

  | L_face, U_face -> [L; F; U'; F'; L']
  | L_face, D_face -> [L'; F; U'; F']
  | L_face, F_face -> [F; U'; F']
  | L_face, B_face -> [B'; U; B]

  | R_face, U_face -> [R'; F'; U; F; R]
  | R_face, D_face -> [R; F'; U; F]
  | R_face, F_face -> [F'; U; F]
  | R_face, B_face -> [B; U'; B']

  | _ -> failwith "invalid face combination when solving white cross"

(* Step 3: *)
(* Spin U until that edge is at UF with white on U *)
let rec spin_u_to_uf (c:cube) (target:colour) : cube * move list =
  let (w_face, o_face) = find_edge_facing c White target in
  match w_face, o_face with
  | U_face, F_face -> (c, [])
  | _ ->
    let c' = apply_move U c in
    let (c2, ms) = spin_u_to_uf c' target in
    (c2, U :: ms)

(* Main solver: White Cross (uses real Y turns between sides) *)
(* assumes that the white is at the bottom *)
let solve_white_cross (c0:cube) : move list =
  let c1, m1 = orient_cube_with_white_down_with_moves c0 in
  let push xs acc = List.rev_append xs acc in
  let rec loop c acc i =
    if i = 4 then List.rev acc
      (* loops 4 times due to the 4 white edge pieces *)
    else
      (* Choose the current target colour: the center on Front *)
      let target = center_of c F_face in
      (* 1) Find the edge with WHITE and that target colour *)
      let (w_face, o_face) = find_edge_facing c White target in
      (* 2) Bring it to U with WHITE on U *)
      let seq1 = steps w_face o_face in
      let c1 = apply_moves seq1 c in

      (* 3) Align it to UF (WHITE on U) by spinning U *)
      let (c2, spin) = spin_u_to_uf c1 target in
      (* 4) Do F2 to insert it into D *)
      let c3 = apply_moves [F; F] c2 in
      (* 5) Do a Y turn to prepare for the next edge *)
      let c4 = apply_move Y c3 in
      let acc' =
        acc
        |> push seq1
        |> push spin
        |> push [F; F] (* F2 *)
        |> push [Y]
      in
      loop c4 acc' (i + 1)
  in
  minimize_moves (loop c1 m1 0)


(* Checker that the cross is done (white on Down + side matches) *)
let is_white_cross_solved (c:cube) : bool =
  let c' = orient_cube_with_white_down c in
  (* all down stickers must be white *)
  eq_colour (center_of c' D_face) White &&
  eq_colour c'.down.top_middle White &&
  eq_colour c'.down.middle_right White &&
  eq_colour c'.down.bottom_middle White &&
  eq_colour c'.down.middle_left White &&
  (* side colours on the middle of each adjacent face must match their centers *)
  eq_colour c'.front.bottom_middle (center_of c' F_face) &&
  eq_colour c'.right.bottom_middle (center_of c' R_face) &&
  eq_colour c'.back.bottom_middle  (center_of c' B_face) &&
  eq_colour c'.left.bottom_middle  (center_of c' L_face)