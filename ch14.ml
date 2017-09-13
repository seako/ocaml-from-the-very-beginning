(* Definitions from the chapter *)

(* these type isn't in the text, but i think it makes the type signatures nicer.
   guess, if i were using an mli i could also avoid the clutter on the inline
   type annotations. *)
type scalar = float
type vector_2d = scalar * scalar

(* make_vector_2d : vector_2d -> vector_2d -> vector_2d *)
let make_vector_2d ((x0, y0) : vector_2d) ((x1, y1) : vector_2d) : vector_2d =
  (x1 -. x0, y1 -. y0)

(* vector_2d_length : vector_2d -> scalar *)
let vector_2d_length ((x, y) : vector_2d) : scalar=
  sqrt (x *. x +. y *. y)

(* offset_point_vector_2d : vector_2d -> vector_2d -> vector_2d *)
let offset_point_vector_2d ((x, y) : vector_2d) ((px, py) : vector_2d) : vector_2d =
  (px +. x, py +. y)

let scale_vector_2d_to_length (l : scalar) ((a, b) : vector_2d) : vector_2d =
  let current_length = vector_2d_length (a, b) in
  if current_length = 0. then (a, b) else
  let factor = l /. current_length in
  (a *. factor, b *. factor)

(* Questions *)
(* 1. Give a function which rounds a positive float to the nearest whole number
   and returns that whole number as a float. *)

let round_to_nearest x =
  let c = ceil x in
  let fl = floor x in
  if c -. x <= x -. fl then c else fl

(* 2. Find the point equedistant between two points *)

let midpoint ((x0, y0) : vector_2d) ((x1, y1) : vector_2d) : vector_2d =
  ((x0 +. x1) /. 2., (y0 +. y1) /. 2.)

(* 3. Write a function to separate a floating point number into its integer
   and fractional parts. *)

let decompose_float x =
  if x < 0. then
    let cl = ceil x in
    (cl, cl -. x)
  else
    let fl = floor x in
    (fl, x -. fl)

(* 4. Write a function `star` of type `float -> unit` which, given a floating-point
   number between zero and one, draws an asterisk to indicate the position. An
   argument of zero will result in an asterisk in column one, and an argument
   of one an asterisk in column fifty. *)

let star x =
  let scaled = floor (x *. 50.) in
  let spaces = int_of_float scaled in
  for i = 1 to (spaces - 1) do
    print_char ' '
  done;
  print_endline "*"

(* 5. Now write a function `plot` which, given a function of type `float -> float`,
   a range, and a step size uses `star`` to draw a graph. *)

let pi = 4. *. (atan 1.)

let rec plot f start finish step =
  if start > finish then ()
  else
    begin
      star (f start);
      plot f (start +. step) finish step;
    end
