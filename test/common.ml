open Rubiks_cube.Cube
(* open Rubiks_cube.Util *)

(* Build a solved cube in your standard color scheme *)
let face_of_colour (c:colour) : face =
  { top_left=c; top_middle=c; top_right=c;
    middle_left=c; middle_middle=c; middle_right=c;
    bottom_left=c; bottom_middle=c; bottom_right=c }

let solved_cube : cube =
  { up    = face_of_colour Yellow;
    down  = face_of_colour White;
    front = face_of_colour Blue;
    back  = face_of_colour Green;
    left  = face_of_colour Orange;
    right = face_of_colour Red }
