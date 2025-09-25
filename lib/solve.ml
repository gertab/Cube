open Rubikscube


(* ---------- Logical face labels (for describing where stickers face) ---------- *)
type face_label =
  | U_face
  | D_face
  | F_face
  | B_face
  | L_face
  | R_face
  
(* ---------- Helpers over your cube ---------- *)
let eq_colour (a:colour) (b:colour) =
  match a,b with
  | White,White | Yellow,Yellow | Red,Red | Orange,Orange | Blue,Blue | Green,Green -> true
  | _ -> false

let center_of (c:cube) = function
  | F_face -> c.front.middle_middle
  | B_face -> c.back.middle_middle
  | L_face -> c.left.middle_middle
  | R_face -> c.right.middle_middle
  | U_face -> c.up.middle_middle
  | D_face -> c.down.middle_middle

(* Edge indexing (12 unique edges) *)
type uedge = UF | UR | UB | UL
type medge = FR | FL | BR | BL
type dedge = DF | DR | DB | DL
type edge_pos = UEdge of uedge | MEdge of medge | DEdge of dedge

let all_edges =
  [ UEdge UF; UEdge UR; UEdge UB; UEdge UL;
    MEdge FR; MEdge FL; MEdge BR; MEdge BL;
    DEdge DF; DEdge DR; DEdge DB; DEdge DL ]

(* The two faces that an edge belongs to, in fixed order *)
let edge_faces = function
  | UEdge UF -> (U_face, F_face)
  | UEdge UR -> (U_face, R_face)
  | UEdge UB -> (U_face, B_face)
  | UEdge UL -> (U_face, L_face)
  | MEdge FR -> (F_face, R_face)
  | MEdge FL -> (F_face, L_face)
  | MEdge BR -> (B_face, R_face)
  | MEdge BL -> (B_face, L_face)
  | DEdge DF -> (D_face, F_face)
  | DEdge DR -> (D_face, R_face)
  | DEdge DB -> (D_face, B_face)
  | DEdge DL -> (D_face, L_face)

(* Read the two stickers’ colours for an edge, aligned with edge_faces order *)
let edge_colours (c:cube) = function
  | UEdge UF -> (c.up.bottom_middle,   c.front.top_middle)
  | UEdge UR -> (c.up.middle_right,    c.right.top_middle)
  | UEdge UB -> (c.up.top_middle,      c.back.top_middle)
  | UEdge UL -> (c.up.middle_left,     c.left.top_middle)
  | MEdge FR -> (c.front.middle_right, c.right.middle_left)
  | MEdge FL -> (c.front.middle_left,  c.left.middle_right)
  | MEdge BR -> (c.back.middle_left,   c.right.middle_right)
  | MEdge BL -> (c.back.middle_right,  c.left.middle_left)
  | DEdge DF -> (c.down.top_middle,    c.front.bottom_middle)
  | DEdge DR -> (c.down.middle_right,  c.right.bottom_middle)
  | DEdge DB -> (c.down.bottom_middle, c.back.bottom_middle)
  | DEdge DL -> (c.down.middle_left,   c.left.bottom_middle)

(* ---------- Move runners (you said you have apply_move; we add a tiny apply_moves) ---------- *)
let rec apply_moves (c:cube) (ms:move list) : cube =
  match ms with [] -> c | m::rest -> apply_moves (apply_move c m) rest

(* ---------- White-cross step table (white-facing × other-facing -> moves) ---------- *)
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

(* ---------- Locate the white+target edge; return (faces the stickers are currently facing) ---------- *)
let find_white_edge_facing (c:cube) (target:colour) : (face_label * face_label) =
  let rec loop = function
    | [] -> failwith "white edge not found"
    | p::ps ->
      let (f1,f2) = edge_faces p in
      let (c1,c2) = edge_colours c p in
      if eq_colour c1 White && eq_colour c2 target then (f1,f2)
      else if eq_colour c2 White && eq_colour c1 target then (f2,f1)
      else loop ps
  in
  loop all_edges

(* ---------- Spin U until that edge is at UF with white on U ---------- *)
let rec spin_u_to_uf (c:cube) (target:colour) : cube * move list =
  let (w_face, o_face) = find_white_edge_facing c target in
  match w_face, o_face with
  | U_face, F_face -> (c, [])
  | _ ->
    let c' = apply_move c U in
    let (c2, ms) = spin_u_to_uf c' target in
    (c2, U :: ms)

(* ---------- Main solver: White Cross (uses real Y turns between sides) ---------- *)
let solve_white_cross (c0:cube) : move list =
  let push xs acc = List.rev_append xs acc in
  let rec loop c acc i =
    if i = 4 then List.rev acc
    else
      let target = center_of c F_face in
      let (w_face, o_face) = find_white_edge_facing c target in
      let seq1 = steps w_face o_face in
      let c1 = apply_moves c seq1 in
      let (c2, spin) = spin_u_to_uf c1 target in
      let c3 = apply_moves c2 [F; F] in
      let c4 = apply_move c3 Y in
      let acc' =
        acc
        |> push seq1
        |> push spin
        |> push [F; F]
        |> push [Y]
      in
      loop c4 acc' (i + 1)
  in
  loop c0 [] 0

(* ---------- Optional: checker that the cross is done (white on Down + side matches) ---------- *)
let cross_solved (c:cube) : bool =
  eq_colour (center_of c D_face) White &&
  eq_colour c.down.top_middle White &&
  eq_colour c.down.middle_right White &&
  eq_colour c.down.bottom_middle White &&
  eq_colour c.down.middle_left White &&
  (* side colours on the middle of each adjacent face must match their centers *)
  eq_colour c.front.bottom_middle (center_of c F_face) &&
  eq_colour c.right.bottom_middle (center_of c R_face) &&
  eq_colour c.back.bottom_middle  (center_of c B_face) &&
  eq_colour c.left.bottom_middle  (center_of c L_face)