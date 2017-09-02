(* Definitions from the chapter *)

type colour =
  |  Red
  | Green
  | Blue
  | Yellow
  | RGB of int * int * int

(* col : colour *)
let col = Blue

(* cols : colour list *)
let cols = [Red ; Red ; Green ; Yellow]

(* colpair : char * colour *)
let colpair = ('R', Red)

(* cols' : colour list *)
let cols' = [Red; Red; Green; Yellow; RGB (150, 0, 255)]

let components colour =
  match colour with
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)
  | RGB (r, g, b) -> (r, g, b)

(* here the definition of type 'a option is introduced but it's already in pervasives *)

(* nothing : 'a option *)
let nothing = None

(* number : int option *)
let number = Some 50

(* numbers : int option list *)
let numbers = [Some 12; None; None; Some 2]

(* word : char list option *)
let word = Some ['c';'a';'k';'e']

(* lookup_opt : 'a -> 'a list -> 'a option *)
let rec lookup_opt x l =
  match l with
  | [] -> None
  | h :: tl -> if h = x then Some h else lookup_opt x tl

type 'a sequence =
  | Nil
  | Cons of 'a * 'a sequence

(* empty_sequence : 'a sequence *)
(* like [] *)
let empty_sequence = Nil

(* singleton_sequence : 'a -> 'a sequence *)
(* like [e] *)
let singleton_sequence e = Cons (e, Nil)

(* sequence_of_list : 'a list -> 'a sequence *)
(* not in the text, I just wanted to write this to make it easier to make sequences *)
let rec sequence_of_list l =
  match l with
  | [] -> Nil
  | h :: tl -> Cons (h, sequence_of_list tl)

(* axe : char sequence *)
(* Cons ('a', Cons ('x', Cons ('e', Nil)))*)
let axe = sequence_of_list ['a';'x';'e']

(* length : 'a sequence -> int *)
let rec length s =
  match s with
  | Nil -> 0
  | Cons (_, tl) -> 1 + length tl

(* append : 'a sequence -> 'a sequence -> 'a sequence *)
let rec append a b =
  match a with
  | Nil -> b
  | Cons (h, tl) -> Cons (h, append tl b)

(* list_of_sequence : 'a sequence -> 'a list *)
(* not in the text, I just thought that since could make a sequence from a list
   I ought also to be able to make a list from a sequence  *)
let rec list_of_sequence s =
  match s with
  | Nil -> []
  | Cons (h, tl) -> h :: list_of_sequence tl

type expr =
  | Num of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Multiply of expr * expr
  | Divide of expr * expr

(* one_plus_two_times_three : expr *)
(* 1 + (2 * 3) *)
let one_plus_two_times_three = Add (Num 1, Multiply (Num 2, Num 3))

(* evaluate : expr -> int *)
let rec evaluate e =
  match e with
  | Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Multiply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e'


(* Questions *)

(* 1. Design a new type `rect` for representing rectangles. Treat squares as a special case. *)

type rect =
  | Square of int
  | Rectangle of int * int (* (width, height) *)

(* 2. Write a function of type `rect -> int` to calculate the area of a given rect. *)
(* width : rect -> int *)
let width r =
  match r with
  | Square s -> s
  | Rectangle (width, _) -> width

(* height : rect -> int *)
let height r =
  match r with
  | Square s -> s
  | Rectangle (_, height) -> height

(* the width and height functions aren't strictly necessary but it seemed like a good idea
   to write functions that make it obvious which part of the rectangle tuple is the width and which is the height *)

(* area : rect -> int *)
let area r = (width r) * (height r)

(* 3. Write a function which rotates a rect such that it is at least as tall as it is wide. *)

(* rotate_upright : rect -> rect *)
let rotate_upright r =
  match r with
  | Square _ -> r
  | Rectangle (s1,s2) ->
    if s2 >= s1 then r
    else Rectangle (s2, s1)

(* 4. Write a function which, given a rect list, returns another list which has the smallest total width and whose
      members are sorted narrowest first. *)

(* tidy_rectangles : rect list -> rect list *)
let tidy_rectangles rs = List.map rotate_upright rs |> List.sort compare

(* 5. write `take`, `drop`, and `map` for the sequence type. *)
let rec take_s n s =
  if n <= 0 then Nil else
    match s with
    | Nil -> Nil
    | Cons (h, tl) -> Cons (h, (take_s (n - 1) tl))

let rec drop_s n s =
  if n <= 0 then s else
    match s with
    | Nil -> Nil
    | Cons (_, tl) -> drop_s (n - 1) tl

let rec map_s f s =
  match s with
  | Nil -> Nil
  | Cons (h, tl) -> Cons (f h, map_s f tl)

(* 6. extend the expr type and evaluate function to allow raising a number to a power *)
type expr' =
  | Num of Num.num (* NOTE: Num is not part of OCaml 4.05 *)
  | Add of expr' * expr'
  | Subtract of expr' * expr'
  | Multiply of expr' * expr'
  | Divide of expr' * expr'
  | Exponentiate of expr' * expr'

let rec evaluate' (e : expr') =
  match e with
  | Num x -> x
  | Add (e, e') -> Num.add_num (evaluate' e) (evaluate' e')
  | Subtract (e, e') -> Num.sub_num (evaluate' e) (evaluate' e')
  | Multiply (e, e') -> Num.mult_num (evaluate' e) (evaluate' e')
  | Divide (e, e') -> Num.div_num (evaluate' e) (evaluate' e')
  | Exponentiate (e, e') -> Num.power_num (evaluate' e) (evaluate' e')

(* 7. Use the option type to deal with the problem that Division_by_zero may be raised
      from the evaluate function *)

let safe_evaluate e =
  try
    Ok (evaluate' e)
  with
  | err -> Error err
