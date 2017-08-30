(* Definitions in the chapter *)

(* the text presents these as two separate functions, 'sort', and, 'insert',
   but it seems to make more sense to define insert as a local function definition
   inside the definition of insertion_sort *)
(* insertion_sort : 'a list -> 'a list *)
let rec insertion_sort l =
  (* insert : 'a -> 'a list -> 'a list *)
  let rec insert x l =
  match l with
  | [] -> [x]
  | h :: tl ->
    if x <= h
    then
      x :: h :: tl
    else
      h :: insert x tl
  in
  match l with
  | [] -> []
  | h :: tl -> insert h (insertion_sort tl)

(* the definitions of take, drop, and lenght from the previous chapter are
   included here as well. *)
(* take : int -> 'a list -> 'a list *)
let rec take n l =
  if n <= 0 then [] else
    match l with
    | [] -> []
    | h :: tl -> h :: take (n - 1) tl

(* drop : int -> 'a list -> 'a list *)
let rec drop n l =
  if n <= 0 then l else
    match l with
    | [] -> []
    | _ :: tl -> drop (n - 1) tl

(* length : 'a list -> int *)
let rec length l =
  match l with
  | [] -> 0
  | _::t -> 1 + length t

(* we inline the inner helper function as we did in the definition of insertion_sort since it seems tidier that way *)
(* merge_sort : 'a list -> a' list *)
let rec merge_sort l =
  (* merge : 'a list -> a' list -> 'a list *)
  let rec merge x y =
    match x, y with
    | [], y -> y
    | x, [] -> x
    | hx :: tlx, hy :: tly ->
      if hx < hy
      then hx :: merge tlx (hy :: tly)
      else hy :: merge (hx :: tlx) tly
  in
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let left = take (length l / 2) l in
    let right = drop (length l / 2) l in
    merge (merge_sort left) (merge_sort right)


(* Questions: *)
(* 1. In merge_sort we calculate the expression length / 2 twice. Modify msort to remove this inefficiency. *)
let rec merge_sort l =
  (* merge : 'a list -> a' list -> 'a list *)
  let rec merge x y =
    match x, y with
    | [], y -> y
    | x, [] -> x
    | hx :: tlx, hy :: tly ->
      if hx < hy
      then hx :: merge tlx (hy :: tly)
      else hy :: merge (hx :: tlx) tly
  in
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let length_of_l = length l / 2 in
    let left = take length_of_l l in
    let right = drop length_of_l l in
    merge (merge_sort left) (merge_sort right)

(* 2. We know that take and drop can fail if called with incorrect arguments.
      Show that this is never the case in merge_sort. *)

(* I imagine the expected proof relies on the fact that length produces an int
   that is greater than or equal to zero and therefore the implementation of merge_sort never calls take and drop
   with negative values for their numeric arguments. This would be true for a numeric type that didn't overflow, but
   this isn't true if the length of the list is greater than `max_int` *)

(* 4. Write a version of insertion_sort that sorts the arguments in reverse order. *)
(* insertion_sort : 'a list -> 'a list *)
let rec insertion_sort_rev l =
  (* insert : 'a -> 'a list -> 'a list *)
  let rec insert x l =
  match l with
  | [] -> [x]
  | h :: tl ->
    if x > h
    then
      x :: h :: tl
    else
      h :: insert x tl
  in
  match l with
  | [] -> []
  | h :: tl -> insert h (insertion_sort_rev tl)

(* 5. How do the comparison functions work for lists?
      Lists are compared based on the ordering of their elements. *)

(* 6. Combine the sort and insert functions into a single sort function. Oh. Too late :) *)
