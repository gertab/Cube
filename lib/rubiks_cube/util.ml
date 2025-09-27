open Cube

(* A solved cube *)
let solved_cube = {
  front = mk_face
    Red Red Red
    Red Red Red
    Red Red Red;
  back = mk_face
    Orange Orange Orange
    Orange Orange Orange
    Orange Orange Orange;
  left = mk_face
    Blue Blue Blue
    Blue Blue Blue
    Blue Blue Blue;
  right = mk_face
    Green Green Green
    Green Green Green
    Green Green Green;
  up = mk_face
    Yellow Yellow Yellow
    Yellow Yellow Yellow
    Yellow Yellow Yellow;
  down = mk_face
    White White White
    White White White
    White White White;
} 

(* Returns a scambling move sequence of given length *) 
let get_scramble (length:int) : move list =
  let () = Random.self_init () in
  let common_moves = [R; R'; U; U'; F; F'] in
  let moves = common_moves @ [U; U'; D; D'; L; L'; R; R'; F; F'; B; B'; X; Y; Z] in
  let rec loop n acc =
    if n = 0 then List.rev acc
    else
      let m = List.nth moves (Random.int (List.length moves)) in
      loop (n-1) (m :: acc)
  in
  loop length []

(* Invert a sequence of moves *)
let invert_moves (ms:move list) : move list =
  let inv m = match m with
    | U -> U' | U' -> U
    | D -> D' | D' -> D
    | F -> F' | F' -> F
    | B -> B' | B' -> B
    | L -> L' | L' -> L
    | R -> R' | R' -> R
    | X -> X' | X' -> X
    | Y -> Y' | Y' -> Y
    | Z -> Z' | Z' -> Z
  in
  List.rev (List.map inv ms)

(* Given a cube and a face, return the color of the center sticker of that face *)
let center_of (c:cube) (fl:face_label) :  colour = 
  match fl with
  | F_face -> c.front.middle_middle
  | B_face -> c.back.middle_middle
  | L_face -> c.left.middle_middle
  | R_face -> c.right.middle_middle
  | U_face -> c.up.middle_middle
  | D_face -> c.down.middle_middle

let all_faces = [U_face; D_face; F_face; B_face; L_face; R_face]

(* Edge indexing (12 unique edges) *)
type edge_pos = 
  | UF | UR | UB | UL (* U layer *)
  | FR | FL | BR | BL (* middle horizontal layer *)
  | DF | DR | DB | DL (* D layer *)

let all_edges =
  [ UF; UR; UB; UL;
    FR; FL; BR; BL;
    DF; DR; DB; DL ]

(* The two faces that an edge belongs to, in fixed order *)
let edge_faces : edge_pos -> face_label * face_label= function
  | UF -> (U_face, F_face)
  | UR -> (U_face, R_face)
  | UB -> (U_face, B_face)
  | UL -> (U_face, L_face)
  | FR -> (F_face, R_face)
  | FL -> (F_face, L_face)
  | BR -> (B_face, R_face)
  | BL -> (B_face, L_face)
  | DF -> (D_face, F_face)
  | DR -> (D_face, R_face)
  | DB -> (D_face, B_face)
  | DL -> (D_face, L_face)

(* Read the two stickers’ colours for an edge, aligned with edge_faces order *)
let edge_colours (c:cube) = function
  | UF -> (c.up.bottom_middle,   c.front.top_middle)
  | UR -> (c.up.middle_right,    c.right.top_middle)
  | UB -> (c.up.top_middle,      c.back.top_middle)
  | UL -> (c.up.middle_left,     c.left.top_middle)
  | FR -> (c.front.middle_right, c.right.middle_left)
  | FL -> (c.front.middle_left,  c.left.middle_right)
  | BR -> (c.back.middle_left,   c.right.middle_right)
  | BL -> (c.back.middle_right,  c.left.middle_left)
  | DF -> (c.down.top_middle,    c.front.bottom_middle)
  | DR -> (c.down.middle_right,  c.right.bottom_middle)
  | DB -> (c.down.bottom_middle, c.back.bottom_middle)
  | DL -> (c.down.middle_left,   c.left.bottom_middle)

(* Used for the cross *)
(* Locate the target1+target2 edge; return (faces the stickers of both targets are currently facing) *)
let find_edge_facing (c:cube) (target1:colour) (target2:colour) : (face_label * face_label) =
  let rec loop = function
    | [] -> failwith ("could not find edge with given colours: " ^ (string_of_colour target1) ^ " and " ^ (string_of_colour target2))
    | p::ps ->
      let (f1,f2) = edge_faces p in
      let (c1,c2) = edge_colours c p in
      if eq_colour c1 target1 && eq_colour c2 target2 then (f1,f2)
      else if eq_colour c2 target1 && eq_colour c1 target2 then (f2,f1)
      else loop ps
  in
  loop all_edges

(* Where is the (a,b) edge now? *)
let find_edge_pos (c:cube) (a:colour) (b:colour) : edge_pos =
  let rec loop = function
    | [] -> failwith "edge not found"
    | p::ps ->
      let (x,y) = edge_colours c p in
      if (eq_colour x a && eq_colour y b) || (eq_colour x b && eq_colour y a)
      then p else loop ps
  in
  loop all_edges

(* Locate the center with the given colour; return its face *)
let find_center_facing (c:cube) (target:colour) : face_label =
  let rec loop = function
    | [] -> failwith ("could not find center with given colour: " ^ (string_of_colour target))
    | f::fs ->
      if eq_colour (center_of c f) target then f
      else loop fs
  in
  loop all_faces


(* Reorient cube so that white center is Down, returning also the moves used *)
let orient_cube_with_white_down_with_moves (c:cube) : cube * move list =
  let white_face = find_center_facing c White in
  match white_face with
  | D_face -> (c, [])
  | U_face -> (apply_move X (apply_move X c), [X; X]) (* X2 *)
  | F_face -> (apply_move X' c, [X'])
  | B_face -> (apply_move X c, [X])
  | L_face -> (apply_move Z' c, [Z'])
  | R_face -> (apply_move Z c, [Z])

(* Reorient cube so that white center is Down *)
let orient_cube_with_white_down (c:cube) : cube =
  orient_cube_with_white_down_with_moves c
  |> fst

(* Corner indexing (8 unique corners) *)
type corner_pos =
  | UFR | URB | UBL | ULF  (* top layer corners *)
  | DFR | DRB | DBL | DLF  (* bottom layer corners *)

let all_corners =
  [ UFR; URB; UBL; ULF;
    DFR; DRB; DBL; DLF ]

(* The three faces that a corner belongs to, in fixed order *)
let corner_faces = function
  | UFR -> (U_face, F_face, R_face)
  | URB -> (U_face, R_face, B_face)
  | UBL -> (U_face, B_face, L_face)
  | ULF -> (U_face, L_face, F_face)
  | DFR -> (D_face, F_face, R_face)
  | DRB -> (D_face, R_face, B_face)
  | DBL -> (D_face, B_face, L_face)
  | DLF -> (D_face, L_face, F_face)

(* Given a cube and a corner, return the colors of its three stickers, aligned with corner_faces order *)
let corner_colours (c:cube) = function
  | UFR -> (c.up.bottom_right,   c.front.top_right,    c.right.top_left)
  | URB -> (c.up.top_right,      c.right.top_right,    c.back.top_left)
  | UBL -> (c.up.top_left,       c.back.top_right,     c.left.top_left)
  | ULF -> (c.up.bottom_left,    c.left.top_right,     c.front.top_left)
  | DFR -> (c.down.top_right,    c.front.bottom_right, c.right.bottom_left)
  | DRB -> (c.down.bottom_right, c.right.bottom_right, c.back.bottom_left)
  | DBL -> (c.down.bottom_left,  c.back.bottom_right,  c.left.bottom_left)
  | DLF -> (c.down.top_left,     c.left.bottom_right,  c.front.bottom_left)

(* Given a cube and three colours, finds the corner with those colours *)
let find_corner (c:cube) (a:colour) (b:colour) (d:colour) : corner_pos =
  let target = [a; b; d] in
  let same_set xs ys =
    List.sort compare xs = List.sort compare ys
  in
  let rec loop = function
    | [] -> failwith "corner not found"
    | pos::rest ->
        let (x,y,z) = corner_colours c pos in
        if same_set [x;y;z] target then pos else loop rest
  in
  loop all_corners

let find_corner_facing (c:cube) (col1:colour) (col2:colour) (col3:colour)
  : face_label * face_label * face_label =
  let pos = find_corner c col1 col2 col3 in
  let (f1,f2,f3) = corner_faces pos in
  let (c1,c2,c3) = corner_colours c pos in (* the order of (c1,c2,c3) may not match (col1,col2,col3) *)
  (* associate each colour with its face *)
  let pairs = [(c1,f1); (c2,f2); (c3,f3)] in
  let face_of col =
    match List.find_opt (fun (c,_) -> c = col) pairs with
    | Some (_,f) -> f
    | None -> failwith "colour not found in corner"
  in
  (face_of col1, face_of col2, face_of col3) (* the order of the faces matches with the colours requested *)


(* Minimizing moves helper functions *)

(* Invert a single move *)
let invert = function
  | U  -> U'  | U' -> U
  | D  -> D'  | D' -> D
  | L  -> L'  | L' -> L
  | R  -> R'  | R' -> R
  | F  -> F'  | F' -> F
  | B  -> B'  | B' -> B
  | X  -> X'  | X' -> X
  | Y  -> Y'  | Y' -> Y
  | Z  -> Z'  | Z' -> Z

(* One linear pass with local rules:
   - m m' (or m' m) -> remove
   - m m m m -> remove
   - m m m -> m'
   (m m stays as-is; no we're not supporting the explicit m2 moves) *)
let simplify_once (ms : move list) : move list =
  let rec step acc = function
    | [] -> List.rev acc
    | m :: rest ->
      match acc with
      | a3 :: a2 :: a1 :: acc_tail when a1 = m && a2 = m && a3 = m ->
          (* four in a row -> drop all four *)
          step acc_tail rest
      | a2 :: a1 :: acc_tail when a1 = m && a2 = m ->
          (* three in a row -> replace by inverse *)
          step (invert m :: acc_tail) rest
      | a1 :: acc_tail when a1 = invert m ->
          (* adjacent inverse pair cancels *)
          step acc_tail rest
      | _ ->
          step (m :: acc) rest
  in
  step [] ms

(* Move minimizer entry point:
   Minimizing moves where possible
   eg: F U U' -> F (adjacent U U' cancels)
       F U U U U -> F (U U U U cancels)
       F U U U -> F U' (three U's become U')
       F U' Y Y Y Y U' U' -> F U (4 Y's cancel, then 3 U' become U) *)
let rec minimize_moves (ms : move list) : move list =
  let ms' = simplify_once ms in
  if ms' = ms then ms else minimize_moves ms'

(* Get all 9 stickers of a face *)
let stickers_of_face (c:cube) = function
  | F_face -> [c.front.top_left; c.front.top_middle; c.front.top_right;
               c.front.middle_left; c.front.middle_middle; c.front.middle_right;
               c.front.bottom_left; c.front.bottom_middle; c.front.bottom_right]
  | B_face -> [c.back.top_left; c.back.top_middle; c.back.top_right;
               c.back.middle_left; c.back.middle_middle; c.back.middle_right;
               c.back.bottom_left; c.back.bottom_middle; c.back.bottom_right]
  | L_face -> [c.left.top_left; c.left.top_middle; c.left.top_right;
               c.left.middle_left; c.left.middle_middle; c.left.middle_right;
               c.left.bottom_left; c.left.bottom_middle; c.left.bottom_right]
  | R_face -> [c.right.top_left; c.right.top_middle; c.right.top_right;
               c.right.middle_left; c.right.middle_middle; c.right.middle_right;
               c.right.bottom_left; c.right.bottom_middle; c.right.bottom_right]
  | U_face -> [c.up.top_left; c.up.top_middle; c.up.top_right;
               c.up.middle_left; c.up.middle_middle; c.up.middle_right;
               c.up.bottom_left; c.up.bottom_middle; c.up.bottom_right]
  | D_face -> [c.down.top_left; c.down.top_middle; c.down.top_right;
               c.down.middle_left; c.down.middle_middle; c.down.middle_right;
               c.down.bottom_left; c.down.bottom_middle; c.down.bottom_right]

let is_solved (c:cube) : bool =
  List.for_all (fun f ->
    let center = center_of c f in
    List.for_all ((=) center) (stickers_of_face c f)
  ) all_faces