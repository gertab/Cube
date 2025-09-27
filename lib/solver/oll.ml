open Rubiks_cube.Cube
open Rubiks_cube.Util

(* CFOP: solving the OLL (Orientation of the Last Layer)

   - Assumes F2L is complete and the cube is oriented with the white center on D.
   - Goal: orient all last-layer stickers so the entire U face is Yellow
           (permutation is ignored; that is done in PLL).
   - Implementation:
       * Pattern -> algorithm lookup (subset of the 57 OLL cases used here).
       * Pattern is a 9-char signature over the U layer:
           order = BLU, BU, BRU, LU, U, RU, FLU, FU, FRU
           each char = facing of the Yellow sticker at that slot (L/B/R/F/U/D).
   - Search strategy:
       * If current orientation isn't in the table, rotate the cube with Y up to 3 times.
       * On a match, apply the corresponding algorithm and include any Y rotations used.
*)

let u2 = [U; U]
let r2 = [R; R]
let x2 = [X; X]
let y2 = [Y; Y]
let m  = [X'; R; L']   (* M  *)
let m' = [L; R'; X]    (* M' *)
let e' = [U'; D; Y]    (* E' *)

(* orientation signature via util
   Pattern order (9 chars): BLU, BU, BRU, LU, U, RU, FLU, FU, FRU.
   Each char is the face where the YELLOW sticker is facing: L/B/R/F/U/D. *)

let face_char = function
  | L_face -> 'L' | B_face -> 'B' | R_face -> 'R'
  | F_face -> 'F' | U_face -> 'U' | D_face -> 'D'

(* Finds the face which yellow is on for a corner piece *)
let face_of_corner_yellow (c:cube) (pos:corner_pos) : face_label =
  let (f1,f2,f3) = corner_faces pos in
  let (c1,c2,c3) = corner_colours c pos in
  if c1 = Yellow then f1
  else if c2 = Yellow then f2
  else if c3 = Yellow then f3
  else U_face (* shouldn't happen on a legal cube; default for safety *)

(* Finds the face which yellow is on for an edge piece *)
let face_of_edge_yellow (c:cube) (pos:edge_pos) : face_label =
  let (f1,f2) = edge_faces pos in
  let (c1,c2) = edge_colours c pos in
  if c1 = Yellow then f1
  else if c2 = Yellow then f2
  else U_face

(* Finds the locations of all yellow pieces on the U face *)
let get_orientations (c0:cube) : string =
  (* OLL expects a consistent orientation; White is on the Down face. *)
  let c = orient_cube_with_white_down c0 in
  let chars = [|
    (* BLU = corner UBL *)
    face_char (face_of_corner_yellow c UBL);
    (* BU  = edge UB   *)
    face_char (face_of_edge_yellow   c UB);
    (* BRU = corner URB *)
    face_char (face_of_corner_yellow c URB);
    (* LU  = edge UL   *)
    face_char (face_of_edge_yellow   c UL);
    (* U-center label is always 'U' for pattern strings *)
    'U';
    (* RU  = edge UR   *)
    face_char (face_of_edge_yellow   c UR);
    (* FLU = corner ULF *)
    face_char (face_of_corner_yellow c ULF);
    (* FU  = edge UF   *)
    face_char (face_of_edge_yellow   c UF);
    (* FRU = corner UFR *)
    face_char (face_of_corner_yellow c UFR);
  |] in
  String.init 9 (Array.get chars)

(* OLL algorithms
   alg_of_pattern returns native move lists (empty list if unknown) 
   [p] contains the orientation of the yellow stickers on the top *)
let steps_oll (p:string) : move list =
  match p with
  | "LBRLURLFR" -> [R; U; B'; X'; R; U] @ x2 @ r2 @ [X'; U'; R'; F; R; F']
  | "LBRLURFFF" -> [R'; F; R; F'] @ u2 @ [R'; F; R; Y'; R; R; U; U; R]
  | "BBRLURLFU" -> [Y] @ m @ [U; X; R'; U; U; X'; R; U; L'; U; L] @ m'
  | "LBULURFFR" -> [R'; U; U; X; R'; U; R; U'; Y; R'; U'; R'; U; R'; F; Z']
  | "UBBLURLFU" -> [R; U; R'; U; R'; F; R; F'] @ u2 @ [R'; F; R; F']
  | "UBULURUFU" -> m' @ u2 @ m @ u2 @ m' @ [U] @ m @ u2 @ m' @ u2 @ m
  | "UBULURLFR" -> [R'; U; U; F; R; U; R'; U'; Y'; R; R; U; U; X'; R; U; X]
  | "BBBLURUFU" -> [F; R; U; R'; U; Y'; R'; U; U; R'; F; R; F']
  | "BURLURFUR" -> [R'; U'; Y; L'; U; L'; Y'; L; F; L'; F; R]
  | "LURLURLUR" -> [R; U'; Y; R; R; D; R'; U; U; R; D'; R; R; Y'; U; R']
  | "BBRUUUFFR" -> [F; U; R; U'; R'; U; R; U'; R'; F']
  | "LBRUUULFR" -> [L'; B'; L; U'; R'; U; R; U'; R'; U; R; L'; B; L]
  | "BURUUUFUR" -> [L; U'; R'; U; L'; U; R; U; R'; U; R]
  | "LURUUULUR" -> [R; U; R'; U; R; U'; R'; U; R; U; U; R']
  | "LUBUUUFUU" -> [L'; U; R; U'; L; U; R']
  | "BURUUULUU" -> [R'; U; U; R; U; R'; U; R]
  | "UUBUUUUUF" -> [R'; F'; L; F; R; F'; L'; F]
  | "UUUUUUFUF" -> [R; R; D; R'; U; U; R; D'; R'; U; U; R']
  | "UUBUUULUU" -> [R'; F'; L'; F; R; F'; L; F]
  | "UBUUURUUU" -> m' @ [U'] @ m @ u2 @ m' @ [U'] @ m
  | "UBUUUUUFU" -> [L'; R; U; R'; U'; L; R'; F; R; F']
  | "BURUURUFF" -> [L; F; R'; F; R; F; F; L']
  | "UURUURFFU" -> [F; R'; F'; R; U; R; U'; R']
  | "LUBUURFFU" -> [R'; U'; R; Y'; X'; R; U'; R'; F; R; U; R'; X]
  | "BUBUURUFU" -> [U'; R; U; U; R'; U'; R; U'; R; R; Y'; R'; U'; R; U; B]
  | "LUBUURLFF" -> [F; R; U; R'; U'; R; U; R'; U'; F']
  | "BUBUURFFF" -> [L; F'; L'; F; U; U; L; L; Y'; L; F; L'; F]
  | "BUBLUUUFU" -> [U'; R'; U; U; R; U; R'; U; R; R; Y; R; U; R'; U'; F']
  | "LUULUUFFR" -> [X; L; U; U; R'; U'; R; U'; X'; L']
  | "BUULUUUFR" -> [R'; U; U; X'; R; R; U'; R'; U; X; R'; U; U; R]
  | "BURLUUFFR" -> [F'; L'; U'; L; U; L'; U'; L; U; F]
  | "LUBLUULFF" -> [R'; F; R'; F'; R; R; U; U; X'; U'; R; U; R'; X]
  | "BUBLUUFFF" -> [R'; F; R; F'; U; U; R; R; Y; R'; F'; R; F']
  | "BBUUURLUF" -> [R; U; R'; Y; R'; F; R; U'; R'; F'; R]
  | "UBBUURFUR" -> [L'; B'; L; U'; R'; U; R; L'; B; L]
  | "LBBUURFUU" -> [U; U; X; L; R; R; U'; R; U'; R'; U; U; R; U'] @ m
  | "UBUUURLUR" -> [X'; U'; R; U'; R; R; F; X; R; U; R'; U'; R; B; B]
  | "LBBLUULUF" -> [L; U'; Y'; R'; U; U; R'; U; R; U'; R; U; U; R; Y; U'; L']
  | "BBRLUUUUF" -> [U; U; X; R'; L; L; U; L'; U; L; U; U; L'; U] @ m
  | "UBULUULUR" -> [Y; Y; F; U; R; U'; X'; U; R'; D'; R; U'; R'; X]
  | "BBRLUULUU" -> [X'; L'; U; U; R; U; R'; U; X; L]
  | "UURLURUUR" -> [R; U; X'; R; U'; R'; U; X; U'; R']
  | "LBRUUUUFU" -> [R; U; R'; U'; X; D'; R'; U; R] @ e' @ [Z']
  | "LBBUUUFFU" -> [R'; F; R; U; R'; F'; R; Y; L; U'; L']
  | "BBRUUUUFF" -> [L; F'; L'; U'; L; F; L'; Y'; R'; U; R]
  | "BBRUUULFU" -> [L'; B'; L; R'; U'; R; U; L'; B; L]
  | "LBBUUUUFR" -> [R; B; R'; L; U; L'; U'; R; B'; R']
  | "UURUURUFR" -> [F; U; R; U'; R'; F']
  | "BUULUUFFU" -> [R'; Y; U'; L; Y'; U; R; U'; R'; F'; R]
  | "UUBUURUFF" -> [L; Y'; U; R'; Y; U'; L'; U; L; F; L']
  | "LUULUULFU" -> [F'; U'; L'; U; L; F]
  | "LBUUUULFU" -> [F; R; U; R'; U'; F']
  | "BBUUUUFFU" -> [R; U; R'; U'; R'; F; R; F']
  | "LBULUUUUF" -> [L; U; L'; U; L; U'; L'; U'] @ y2 @ [R'; F; R; F']
  | "UBRUURFUU" -> [R'; U'; R; U'; R'; U; R; U; Y; F; R'; F'; R]
  | "UBBUUULFU" -> [R'; F; R; U; R'; U'; Y; L'; Y'; U; R]
  | "BBUUUUUFR" -> [L; F'; L'; U'; L; U; Y'; R; Y; U'; L']
  | _ -> []

(* Checks whether OLL is done (all yellow on Up) *)
let is_oll_solved (c : cube) : bool =
  let c' = orient_cube_with_white_down c in
  let u = c'.up in
  let all_yellows =
    u.top_left = Yellow
    && u.top_middle = Yellow
    && u.top_right = Yellow
    && u.middle_left = Yellow
    && u.middle_middle = Yellow
    && u.middle_right = Yellow
    && u.bottom_left = Yellow
    && u.bottom_middle = Yellow
    && u.bottom_right = Yellow
  in
  all_yellows


let solve_oll (c0:cube) : move list =
  let is_white_center_down = eq_colour (center_of c0 D_face) White in
  if not (F2l.is_f2l_solved c0) then
    failwith "[oll] cube does not have F2L solved"
  else if not is_white_center_down then
    failwith "[oll] cube not oriented with white center on Down face"
  else if is_oll_solved c0 then
    []  (* already solved *)
  else 
    (* Try current + up to 3 Y rotations to match a known pattern.
       If none match, we just return []. *)
    let rec try4 c acc k =
      if k = 4 then []
      else
        let key = get_orientations c in
        let alg = steps_oll key in
        match alg with
        | [] ->
            let c' = apply_move Y c in
            try4 c' (Y :: acc) (k+1)
        | ms ->
            List.rev acc @ ms
    in
     (try4 c0 [] 0)