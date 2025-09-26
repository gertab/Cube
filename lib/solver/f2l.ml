(* lib/solver/f2l.ml *)
open Rubiks_cube.Cube
open Rubiks_cube.Util

(* CFOP: solving the First Two Layers (F2L)
   - With the white cross already solved (White on the Down face) and any front color facing you, solve the four corner–edge pairs that belong between the Down layer and the middle layer:
       DFR + FR, DRB + RB, DBL + BL, DLF + LF.
   - One F2L pair consists of:
       - the WHITE–FRONT–RIGHT corner (DFR target), and
       - the FRONT–RIGHT edge (FR target).

   Steps to solve one F2L pair:
   1) Identify the current pair colors from centers: F := center_of F_face, R := center_of R_face.
   2) Find the matching WHITE–F–R corner and the F–R edge on the cube.
   3) Corner "first step":
      - Using the small FIRST_STEP table, bring the corner to an easy working position
        (U-layer at UFR or directly to DFR) based on its current position and where WHITE faces.
   4) Edge nudge (special cases):
      - If the F–R edge is trapped in BL / BR / FL, free it with a short bump-out sequence.
   5) Corner alignment:
      - If the corner is on the U layer but not at UFR, spin U until it sits at UFR.
   6) Pairing & insertion:
      - Compute the (corner facings: where its F/R/WHITE stickers point) and
        the (edge facings: where its F/R stickers point).
      - Look up the algorithm in the F2L STEPS table and apply it to pair and insert
        the corner+edge into their slot (DFR+FR in this orientation).
   7) Rotate Y:
      - Apply a Y turn to make the next unsolved pair become the new FRONT/RIGHT.

   - Repeat exactly four times; each iteration solves one pair and then performs Y.
*)

let u2 = [U; U]

(* First Step: (bring the White–Front–Right corner to UFR/DFR) *)
let first_step_moves (goal_cubie : corner_pos) (white_facing : face_label) : move list =
  match goal_cubie with
  | DFR -> begin  (* 'DFR' *)
      match white_facing with
      | F_face -> [R; U'; R']
      | R_face -> [R; U; R'; U']
      | _ -> []
    end

  | DLF -> begin  (* 'DFL' *)
      match white_facing with
      | F_face -> [L'; U; L; U']
      | L_face -> [L'; U'; L]
      | D_face -> [L'; U'; L]
      | _ -> []
    end

  | DBL -> begin  (* 'BDL' *)
      match white_facing with
      | B_face -> [B'] @ u2 @ [B]
      | D_face -> [B'] @ u2 @ [B]
      | L_face -> [B'; U; B] @ u2
      | _ -> []
    end

  | DRB -> begin  (* 'DRB' *)
      match white_facing with
      | B_face -> [B; U; B']
      | D_face -> [B; U; B']
      | R_face -> [B; U'; B'; U]
      | _ -> []
    end

  | URB -> begin  (* 'BRU' (U-layer) *)
      match white_facing with
      | B_face | R_face | U_face -> [U]
      | _ -> []
    end

  | UBL -> begin  (* 'BLU' *)
      match white_facing with
      | B_face | L_face | U_face -> u2
      | _ -> []
    end

  | ULF -> begin  (* 'FLU' *)
      match white_facing with
      | F_face | L_face | U_face -> [U']
      | _ -> []
    end

  | UFR -> []

(* After the first step, if the corner is in U layer, spin U to place it at UFR *)
let rec spin_u_until_corner_at_ufr (c:cube) (fcol:colour) (rcol:colour) : cube * move list =
  let pos = find_corner c fcol rcol White in
  match pos with
  | UFR -> (c, [])
  | URB | UBL | ULF ->
      let c' = apply_move U c in
      let (c2, ms) = spin_u_until_corner_at_ufr c' fcol rcol in
      (c2, U :: ms)
  | _ -> (c, [])  (* already placed at DFR, or somewhere we won't spin for *)

(* Edge bump-out from back/left (only for BL/BR/FL) *)
let bump_edge_out_if_needed (c:cube) (fcol:colour) (rcol:colour) : cube * move list =
  match find_edge_pos c fcol rcol with
  | BL -> (apply_moves [B'; U'; B] c, [B'; U'; B])
  | BR -> (apply_moves [B; U; B'] c, [B; U; B'])
  | FL -> (apply_moves [L'; U'; L] c, [L'; U'; L])
  | _  -> (c, [])

(* ------------------------------------------------------------------ *)
(* F2L main STEPS table ( in native moves)    *)
(* Keys: corner facings (F,R,W) * edge facings (F,R) -> move list     *)
(* https://ruwix.com/the-rubiks-cube/advanced-cfop-fridrich/first-two-layers-f2l/ *)
(* ------------------------------------------------------------------ *)

let u2 = [U; U]

(* F2L STEPS: (corner facings: F,R,White) × (edge facings: F,R) -> moves *)
let get_step
    ((cf, cr, cw) : face_label * face_label * face_label)
    ((ef, er)     : face_label * face_label)
  : move list =
  match (cf, cr, cw) with
  (* -------------------- 'FUR' -------------------- *)
  | (F_face, U_face, R_face) -> begin
      match (ef, er) with
      | (U_face, B_face) -> [R; U; R']
      | (F_face, U_face) -> [U'; F'; U; F]
      | (F_face, R_face) -> [U; F'; U; F; U; F'] @ u2 @ [F]
      | (R_face, F_face) -> [U; F'; U'; F; Y; U'; F; U; F'; Y']
      | (R_face, U_face) -> [R; U'; R'; U; Y'; U; R'; U'; R; Y]
      | (B_face, U_face) -> [U; F'] @ u2 @ [F; U; F'] @ u2 @ [F]
      | (L_face, U_face) -> [U; F'; U'; F; U; F'] @ u2 @ [F]
      | (U_face, R_face) -> [U'; R; U'; R'; U; R; U; R']
      | (U_face, L_face) -> [U'; R; U; R'; U; R; U; R']
      | (U_face, F_face) -> [U; F'] @ u2 @ [F; U'; R; U; R']
      | _ -> []
    end

  (* -------------------- 'URF' -------------------- *)
  | (U_face, R_face, F_face) -> begin
      match (ef, er) with
      | (L_face, U_face) -> [F'; U'; F]
      | (U_face, R_face) -> [U; R; U'; R']
      | (F_face, R_face) -> [U'; R; U'; R'; U'; R] @ u2 @ [R']
      | (R_face, F_face) -> [U'; R; U; R'; Y'; U; R'; U'; R; Y]
      | (U_face, F_face) -> [F'; U; F; U'; Y; U'; F; U; F'; Y']
      | (U_face, L_face) -> [U'; R] @ u2 @ [R'; U'; R] @ u2 @ [R']
      | (U_face, B_face) -> [U'; R; U; R'; U'; R] @ u2 @ [R']
      | (F_face, U_face) -> [U; F'; U; F; U'; F'; U'; F]
      | (B_face, U_face) -> [U; F'; U'; F; U'; F'; U'; F]
      | (R_face, U_face) -> [U'; R] @ u2 @ [R'; U; F'; U'; F]
      | _ -> []
    end

  (* -------------------- 'FRD' -------------------- *)
  | (F_face, R_face, D_face) -> begin
      match (ef, er) with
      | (F_face, U_face) -> [U; R; U'; R'; U'; F'; U; F]
      | (R_face, U_face) -> u2 @ [R; U'; R'; U'; F'; U; F]
      | (L_face, U_face) -> [R; U'; R'; U'; F'; U; F]
      | (B_face, U_face) -> [U'; R; U'; R'; U'; F'; U; F]
      | (U_face, R_face) -> [U'; F'; U; F; U; R; U'; R']
      | (U_face, L_face) -> [U; F'; U; F; U; R; U'; R']
      | (U_face, B_face) -> [F'; U; F; U; R; U'; R']
      | (U_face, F_face) -> u2 @ [F'; U; F; U; R; U'; R']
      | (R_face, F_face) -> [R; U'; R'; Y'; U; R'; U; U; R; U; R'; U; U; R; Y]
      | (F_face, R_face) -> []
      | _ -> []
    end

  (* -------------------- 'DFR' -------------------- *)
  | (D_face, F_face, R_face) -> begin
      match (ef, er) with
      | (F_face, U_face) ->
          [F'; U; F; U'; F'; U; F]
      | (U_face, R_face) ->
          [R; U; R'; U'; R; U; R']
      | (F_face, R_face) ->
          [R; U'; R'; U; R] @ u2 @ [R'; U; R; U'; R']
      | (R_face, F_face) ->
          [R; U; R'; U'; R; U'; R'; U; Y'; U; R'; U'; R; Y]
      | _ -> []
    end

  (* -------------------- 'RDF' -------------------- *)
  | (R_face, D_face, F_face) -> begin
      match (ef, er) with
      | (F_face, U_face) ->
          [F'; U'; F; U; F'; U'; F]
      | (U_face, R_face) ->
          [R; U'; R'; U; R; U'; R']
      | (F_face, R_face) ->
          [R; U'; R'; U'; R; U; R'; U'; R] @ u2 @ [R']
      | (R_face, F_face) ->
          [R; U'; R'; Y'; U; R'; U'; R; U'; R'; U'; R; Y]
      | _ -> []
    end

  (* -------------------- 'RFU' -------------------- *)
  | (R_face, F_face, U_face) -> begin
      match (ef, er) with
      | (F_face, R_face) ->
          [R; U; R'; U'; R; U; R'; U'; R; U; R']
      | (R_face, F_face) ->
          [R; U'; R'; Y'; U; R'; U; R; Y]
      | (U_face, F_face) ->
          [R; U; R'; U'] @ u2 @ [R; U; R'; U'; R; U; R']
      | (U_face, L_face) ->
          u2 @ [R; U; R'; U; R; U'; R']
      | (U_face, B_face) ->
          [U; R] @ u2 @ [R'; U; R; U'; R']
      | (U_face, R_face) ->
          [R] @ u2 @ [R'; U'; R; U; R']
      | (L_face, U_face) ->
          [U'; F'] @ u2 @ [F; U'; F'; U; F]
      | (B_face, U_face) ->
          u2 @ [F'; U'; F; U'; F'; U; F]
      | (R_face, U_face) ->
          [Y'; R'; U'; R; U; U; R'; U'; R; U; R'; U'; R; Y]
      | (F_face, U_face) ->
          [F'] @ u2 @ [F; U; F'; U'; F]
      | _ -> []
    end

  | _ -> []

(* ------------------------------------------------------------------ *)
(* Fix one F2L pair (the full "remaining steps" after first step)     *)
(* ------------------------------------------------------------------ *)

let f2l_one_pair (c:cube) : cube * move list =
  (* Current pair colours from centers *)
  let front_color = center_of c F_face in
  let right_color = center_of c R_face in

  (* let () = Printf.printf "Solving F2L pair (%s, %s)\n"
      (string_of_colour front_color) (string_of_colour right_color) in *)

  (* Step A: FIRST_STEP for the corner *)
  let corner_pos = find_corner c front_color right_color White in
  let (_f_on, _r_on, w_on) = find_corner_facing c front_color right_color White in
  let ms_first = first_step_moves corner_pos w_on in
  (* let () = Printf.printf "  First step moves: %s\n"
      (String.concat " " (List.map string_of_move ms_first)) in *)
  let c1 = apply_moves ms_first c in

  (* Step B: Edge special-case "bump out" if needed *)
  let (c2, ms_bump) = bump_edge_out_if_needed c1 front_color right_color in

  (* Step C: If the corner is in U-layer, spin U to bring it to UFR *)
  let (c3, ms_spin_corner) = spin_u_until_corner_at_ufr c2 front_color right_color in

  (* Step D: Compute facings and run the F2L step procudure *)
  let corner_facings = find_corner_facing c3 front_color right_color White in
  let edge_facings   = find_edge_facing   c3 front_color right_color in
  let ms_table       = get_step corner_facings edge_facings in
  (* let () = Printf.printf "  F2L step moves: %s\n"
      (String.concat " " (List.map string_of_move ms_table)) in *)
  let c4             = apply_moves ms_table c3 in

  (* Step E: rotate Y to move to the next pair *)
  let c5 = apply_move  Y c4 in
  let ms_total =
    ms_first @ ms_bump @ ms_spin_corner @ ms_table @ [Y]
  in
  (c5, ms_total)

(* Solve all 4 F2L pairs *)
let solve_f2l (c0:cube) : move list =
  let is_white_center_down = eq_colour (center_of c0 D_face) White in
  if not is_white_center_down then
    failwith "[f2l] cube not oriented with white center on Down face"
  else 
    let rec loop i c acc =
      (* Printf.printf "F2L step %d\n%s\n" (i+1) (string_of_cube c); *)
      if i = 4 then acc
      else
        let (c', ms) = f2l_one_pair c in
        loop (i+1) c' (acc @ ms)
    in
    minimize_moves (loop 0 c0 [])

    
(* Check if F2L is solved (first two layers complete) *)
let is_f2l_solved (c : cube) : bool =
  (* Orient so White center is on Down (uses whole-cube rotations; doesn't scramble) *)
  let c' = orient_cube_with_white_down c in

  (* Centers for quick comparisons *)
  let fC = center_of c' F_face
  and rC = center_of c' R_face
  and bC = center_of c' B_face
  and lC = center_of c' L_face in

  (* Corner checks (Down sticker must be White; side stickers match their centers) *)
  let corner_dfr_ok =
    c'.down.top_right          = White &&
    c'.front.bottom_right      = fC    &&
    c'.right.bottom_left       = rC
  in
  let corner_drb_ok =
    c'.down.bottom_right       = White &&
    c'.right.bottom_right      = rC    &&
    c'.back.bottom_left        = bC
  in
  let corner_dbl_ok =
    c'.down.bottom_left        = White &&
    c'.back.bottom_right       = bC    &&
    c'.left.bottom_left        = lC
  in
  let corner_dlf_ok =
    c'.down.top_left           = White &&
    c'.left.bottom_right       = lC    &&
    c'.front.bottom_left       = fC
  in

  (* Edge checks (middle layer edges match their two centers) *)
  let edge_matches pos (fa, fb) =
    let (a, b) = edge_colours c' pos in
    (a = fa && b = fb) || (a = fb && b = fa)
  in
  let edge_fr_ok = edge_matches FR (fC, rC) in
  let edge_br_ok = edge_matches BR (bC, rC) in
  let edge_bl_ok = edge_matches BL (bC, lC) in
  let edge_fl_ok = edge_matches FL (fC, lC) in

  corner_dfr_ok && corner_drb_ok && corner_dbl_ok && corner_dlf_ok
  && edge_fr_ok && edge_br_ok && edge_bl_ok && edge_fl_ok