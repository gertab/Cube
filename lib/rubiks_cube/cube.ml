(* Rubik's Cube *)

type colour = 
  | White
  | Yellow
  | Red
  | Orange
  | Blue
  | Green

type face = {
  top_left      : colour;
  top_middle    : colour;
  top_right     : colour;
  middle_left   : colour;
  middle_middle : colour;
  middle_right  : colour;
  bottom_left   : colour;
  bottom_middle : colour;
  bottom_right  : colour;
}

type cube = {
  front : face;
  back  : face;
  left  : face;
  right : face;
  up    : face;
  down  : face;
}

(* Logical face labels (for describing where stickers face) *)
type face_label =
  | U_face
  | D_face
  | F_face
  | B_face
  | L_face
  | R_face

let string_of_colour : colour -> string = function 
  | White  -> "W"
  | Yellow -> "Y"
  | Red    -> "R"
  | Orange -> "O"
  | Blue   -> "B"
  | Green  -> "G"

let eq_colour (a: colour) (b:colour) =
  match a,b with
  | White,White | Yellow,Yellow | Red,Red | Orange,Orange | Blue,Blue | Green,Green -> true
  | _ -> false

let string_of_cube (c : cube) : string =
  let string_of_row (a,b,c) =
    string_of_colour a ^ string_of_colour b ^ string_of_colour c in
    
  let row_top f    = (f.top_left,    f.top_middle,    f.top_right) in
  let row_middle f = (f.middle_left, f.middle_middle, f.middle_right) in
  let row_bottom f = (f.bottom_left, f.bottom_middle, f.bottom_right) in
  let sep = " " in
  let pad = String.make 4 ' ' in
  let t1 = pad ^ string_of_row (row_top    c.up) in
  let t2 = pad ^ string_of_row (row_middle c.up) in
  let t3 = pad ^ string_of_row (row_bottom c.up) in

  let m1 =
    String.concat sep [
      string_of_row (row_top    c.left);
      string_of_row (row_top    c.front);
      string_of_row (row_top    c.right);
      string_of_row (row_top    c.back);
    ]
  in
  let m2 =
    String.concat sep [
      string_of_row (row_middle c.left);
      string_of_row (row_middle c.front);
      string_of_row (row_middle c.right);
      string_of_row (row_middle c.back);
    ]
  in
  let m3 =
    String.concat sep [
      string_of_row (row_bottom c.left);
      string_of_row (row_bottom c.front);
      string_of_row (row_bottom c.right);
      string_of_row (row_bottom c.back);
    ]
  in

  let b1 = pad ^ string_of_row (row_top    c.down) in
  let b2 = pad ^ string_of_row (row_middle c.down) in
  let b3 = pad ^ string_of_row (row_bottom c.down) in

  String.concat "\n" [t1; t2; t3; m1; m2; m3; b1; b2; b3] ^ "\n"  


type move = 
  | U  (* Up clockwise *)
  | U' (* Up counter-clockwise *)
  | D (* Down/bottom clockwise *)
  | D'
  | L
  | L'
  | R
  | R'
  | F
  | F'
  | B  (* Back clockwise *)
  | B'
  | X (* rotate whole cube like R *)
  | X'
  | Y (* rotate whole cube like U *)
  | Y'
  | Z (* rotate whole cube like F *)
  | Z'

let string_of_move : move -> string = function
  | U  -> "U"
  | U' -> "U'"
  | D  -> "D"
  | D' -> "D'"
  | L  -> "L"
  | L' -> "L'"
  | R  -> "R"
  | R' -> "R'"
  | F  -> "F"
  | F' -> "F'"
  | B  -> "B"
  | B' -> "B'"
  | X  -> "X"
  | X'  -> "X'"
  | Y  -> "Y"
  | Y'  -> "Y'"
  | Z  -> "Z"
  | Z'  -> "Z'"


let mk_face tl tm tr ml mm mr bl bm br = {
  top_left = tl; top_middle = tm; top_right = tr;
  middle_left = ml; middle_middle = mm; middle_right = mr;
  bottom_left = bl; bottom_middle = bm; bottom_right = br;
}

(* Helpers functions for apply_move *)


(* Rotate face clockwise *)
let rotate_face_cw (f: face) : face =
  (* 0 1 2      6 3 0
     3 4 5  ->  7 4 1
     6 7 8      8 5 2 *)
  mk_face
    f.bottom_left  f.middle_left   f.top_left
    f.bottom_middle f.middle_middle f.top_middle
    f.bottom_right f.middle_right  f.top_right

(* Rotate face counter-clockwise *)
let rotate_face_ccw (f: face) : face =
  (* ccw = cw three times, but do it directly for clarity *)
  (* 0 1 2      2 5 8
     3 4 5  ->  1 4 7
     6 7 8      0 3 6 *)
  mk_face
    f.top_right   f.middle_right  f.bottom_right
    f.top_middle  f.middle_middle f.bottom_middle
    f.top_left    f.middle_left   f.bottom_left

(* Row/column getters and setters *)
let top_row f = (f.top_left, f.top_middle, f.top_right)
let mid_row f = (f.middle_left, f.middle_middle, f.middle_right)
let bot_row f = (f.bottom_left, f.bottom_middle, f.bottom_right)

let left_col f = (f.top_left, f.middle_left, f.bottom_left)
let mid_col  f = (f.top_middle, f.middle_middle, f.bottom_middle)
let right_col f = (f.top_right, f.middle_right, f.bottom_right)

let set_top_row f (a,b,c) =
  { f with top_left = a; top_middle = b; top_right = c }
let set_mid_row f (a,b,c) =
  { f with middle_left = a; middle_middle = b; middle_right = c }
let set_bot_row f (a,b,c) =
  { f with bottom_left = a; bottom_middle = b; bottom_right = c }

let set_left_col f (a,b,c) =
  { f with top_left = a; middle_left = b; bottom_left = c }
let set_mid_col f (a,b,c) =
  { f with top_middle = a; middle_middle = b; bottom_middle = c }
let set_right_col f (a,b,c) =
  { f with top_right = a; middle_right = b; bottom_right = c }

(* Reverse a triple (useful for orientation when moving columns into rows) *)
let rev3 (a,b,c) = (c,b,a)

let mirror_face_twice (f: face) : face =
  mk_face
    f.bottom_right f.bottom_middle f.bottom_left
    f.middle_right f.middle_middle f.middle_left
    f.top_right    f.top_middle    f.top_left

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


(* nice to implement -- not obvious with llm/makes precision mistakes *)
let rec apply_move (m: move) (c: cube) : cube =
match m with
  (* ---- U: rotate top face CW; cycle top rows F->L->B->R ---- *)
  | U ->
    let up' = rotate_face_cw c.up in
    let f_top = top_row c.front
    and r_top = top_row c.right
    and b_top = top_row c.back
    and l_top = top_row c.left in
    let front' = set_top_row c.front r_top in
    let right' = set_top_row c.right b_top in
    let back'  = set_top_row c.back  l_top in
    let left'  = set_top_row c.left  f_top in
    { c with up = up'; front = front'; right = right'; back = back'; left = left' }

  (* ---- U': rotate top face CCW; cycle top rows F->R->B->L ---- *)
  | U' ->
    let up' = rotate_face_ccw c.up in
    let f_top = top_row c.front
    and r_top = top_row c.right
    and b_top = top_row c.back
    and l_top = top_row c.left in
    let front' = set_top_row c.front l_top in
    let right' = set_top_row c.right f_top in
    let back'  = set_top_row c.back  r_top in
    let left'  = set_top_row c.left  b_top in
    { c with up = up'; front = front'; right = right'; back = back'; left = left' }

  (* ---- D: rotate bottom face CW; cycle bottom rows F->R->B->L ---- *)
  | D ->
    let down' = rotate_face_cw c.down in
    let f_bot = bot_row c.front
    and r_bot = bot_row c.right
    and b_bot = bot_row c.back
    and l_bot = bot_row c.left in
    let front' = set_bot_row c.front l_bot in
    let left'  = set_bot_row c.left  b_bot in
    let back'  = set_bot_row c.back  r_bot in
    let right' = set_bot_row c.right f_bot in
    { c with down = down'; front = front'; left = left'; back = back'; right = right' }

  (* ---- D': rotate bottom face CCW; inverse cycle F->L->B->R ---- *)
  | D' ->
    let down' = rotate_face_ccw c.down in
    let f_bot = bot_row c.front
    and r_bot = bot_row c.right
    and b_bot = bot_row c.back
    and l_bot = bot_row c.left in
    let front' = set_bot_row c.front r_bot in
    let left'  = set_bot_row c.left  f_bot in
    let back'  = set_bot_row c.back  l_bot in
    let right' = set_bot_row c.right b_bot in
    { c with down = down'; front = front'; left = left'; back = back'; right = right' }
      (* ---- L: rotate left face CW; cycle left columns F<-B<-D<-U ---- *)
  | L ->
    let left' = rotate_face_cw c.left in
    let u_l = left_col c.up
    and f_l = left_col c.front
    and d_l = left_col c.down
    and b_l = right_col c.back in
    let front'  = set_left_col  c.front  u_l in
    let back'   = set_right_col  c.back   (rev3 d_l) in
    let down' = set_left_col  c.down f_l in
    let up'    = set_left_col  c.up    (rev3 b_l) in
    { c with left = left'; front = front'; back = back'; down = down'; up = up' }

  (* ---- L': inverse: left columns F<-U<-D<-B ---- *)
  | L' ->
    let left' = rotate_face_ccw c.left in
    let u_l = left_col c.up
    and f_l = left_col c.front
    and d_l = left_col c.down
    and b_r = right_col c.back in
    let front'  = set_left_col  c.front  d_l in
    let up'    = set_left_col  c.up    f_l in
    let down' = set_left_col  c.down (rev3 b_r) in
    let back'   = set_right_col  c.back   (rev3 u_l) in
    { c with left = left'; front = front'; up = up'; down = down'; back = back' }

  (* ---- R: rotate right face CW; cycle right columns ---- *)
  | R ->
    let right' = rotate_face_cw c.right in
    let u_r = right_col c.up
    and f_r = right_col c.front
    and d_r = right_col c.down
    and b_l = left_col c.back in
    let front'  = set_right_col c.front  d_r in
    let back'   = set_left_col  c.back   (rev3 u_r) in
    let down' = set_right_col c.down (rev3 b_l) in
    let up'    = set_right_col c.up    f_r in
    { c with right = right'; front = front'; back = back'; down = down'; up = up' }

  (* ---- R': inverse of R---- *)
  | R' ->
    let right' = rotate_face_ccw c.right in
    let u_r = right_col c.up
    and f_r = right_col c.front
    and d_r = right_col c.down
    and b_l = left_col c.back in
    let front'  = set_right_col c.front  u_r in
    let up'    = set_right_col c.up (rev3 b_l) in
    let down' = set_right_col c.down f_r in
    let back'   = set_left_col c.back   (rev3 d_r) in
    { c with right = right'; front = front'; up = up'; down = down'; back = back' }

  (* ---- F: rotate front face CW ---- *)
  | F ->
    let front' = rotate_face_cw c.front in
    let u_b = bot_row  c.up
    and r_l = left_col c.right
    and d_t = top_row  c.down
    and l_r = right_col c.left in
    let up'    = set_bot_row   c.up    (rev3 l_r) in
    let left'   = set_right_col c.left   d_t in
    let down' = set_top_row   c.down (rev3 r_l) in
    let right'  = set_left_col  c.right  u_b in
    { c with front = front'; up = up'; left = left'; down = down'; right = right' }

  (* ---- F': inverse of F ---- *)
  | F' ->
    let front' = rotate_face_ccw c.front in
    let u_b = bot_row  c.up
    and r_l = left_col c.right
    and d_t = top_row  c.down
    and l_r = right_col c.left in
    let up'    = set_bot_row   c.up    r_l in
    let right'  = set_left_col  c.right  (rev3 d_t) in
    let down' = set_top_row   c.down l_r in
    let left'   = set_right_col c.left   (rev3 u_b) in
    { c with front = front'; up = up'; right = right'; down = down'; left = left' }

  (* ---- B: rotate back face CW ---- *)
  | B ->
    let back' = rotate_face_cw c.back in
    let u_t = top_row   c.up
    and r_r = right_col c.right
    and d_b = bot_row   c.down
    and l_l = left_col  c.left in
    let up'    = set_top_row   c.up    r_r in
    let right'  = set_right_col c.right  (rev3 d_b) in
    let down' = set_bot_row   c.down l_l in
    let left'   = set_left_col  c.left   (rev3 u_t) in
    { c with back = back'; up = up'; right = right'; down = down'; left = left' }

  (* ---- B': inverse: U.top<-L.left<-D.down<-R.right ---- *)
  | B' ->
    let back' = rotate_face_ccw c.back in
    let u_t = top_row   c.up 
    and r_r = right_col c.right
    and d_b = bot_row   c.down
    and l_l = left_col  c.left in
    let up'    = set_top_row   c.up    (rev3 l_l) in
    let left'   = set_left_col  c.left   d_b in
    let down' = set_bot_row   c.down (rev3 r_r) in
    let right'  = set_right_col c.right  u_t in
    { c with back = back'; up = up'; left = left'; down = down'; right = right' }
  | X -> (* rotate cube like R *)
    let front' = c.down in
    let up' = c.front in
    let back' = mirror_face_twice c.up in
    let down' = mirror_face_twice c.back in
    let right' = rotate_face_cw c.right in
    let left' = rotate_face_ccw c.left in
    { front = front'; up = up'; back = back'; down = down'; right = right'; left = left' }
  | Y -> (* rotate cube like U *)
    let front' = c.right in
    let right' = c.back in
    let back' = c.left in
    let left' = c.front in
    let up' = rotate_face_cw c.up in
    let down' = rotate_face_ccw c.down in
    { front = front'; right = right'; back = back'; left = left'; up = up'; down = down' }
  | Z -> (* rotate cube like F *)
    let up' = rotate_face_cw c.left in
    let right' = rotate_face_cw c.up in
    let down' = rotate_face_cw c.right in
    let left' = rotate_face_cw c.down in
    let front' = rotate_face_cw c.front in
    let back' = rotate_face_ccw c.back in
    { up = up'; right = right'; down = down'; left = left'; front = front'; back = back' }
  | X' -> (* inverse of X *)
    c
    |> apply_move X
    |> apply_move X
    |> apply_move X
    
    (* apply_move (apply_move (apply_move c X) X) X *)
  | Y' -> (* inverse of Y *)
    c
    |> apply_move Y
    |> apply_move Y
    |> apply_move Y  
  | Z' -> (* inverse of Z *)
    c
    |> apply_move Z
    |> apply_move Z
    |> apply_move Z
let apply_moves (ms: move list) (c: cube) : cube =
  List.fold_left (fun acc m -> apply_move m acc) c ms
  (* List.fold_left apply_move ms c *)







