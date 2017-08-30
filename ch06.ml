(* Definitions from the chapter *)

(* double : int list -> int list *)
let rec double l =
  match l with
  | [] -> []
  | h :: tl -> (h * 2) :: double tl

(* evens : int list -> bool list *)
let rec evens l =
  match l with
  | [] -> []
  | h :: tl -> (h mod 2 = 0) :: evens tl

(* map : (a -> b) -> 'a list -> 'b list *)
let rec map f l =
  match l with
  | [] -> []
  | h :: tl -> f h :: map f tl

(* halve : int -> int *)
let halve x = x / 2

(* halve_elements : int list -> int list *)
let halve_elements = map halve

(* is_even : int -> bool *)
let is_even x = x mod 2 = 0

(* evens' : int list -> bool list *)
let evens' = map is_even

(* evens'' : int list -> bool list *)
let evens'' = map (fun x -> x mod 2 = 0)


(* the definitions of take, drop, and lenght from the previous chapters are
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

(* merge : ('a -> 'a -> bool) 'a list -> 'a list -> 'a list*)
let rec merge cmp x y =
  match x, y with
  | [], l -> l
  | l, [] -> l
  | hx :: tx, hy :: ty ->
    if cmp hx hy
    then hx :: merge cmp tx (hy :: ty)
    else hy :: merge cmp (hx :: tx) ty

(* msort : ('a -> 'a -> bool) -> 'a list -> 'a list *)
let rec msort cmp l =
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let n = (length l / 2) in
    let left = take n l in
    let right = drop n l in
    merge cmp (msort cmp left) (msort cmp right)

let sort_ascending = msort (<=)

let sort_descending = msort (>=)


(* Questions *)

(* 1. Write a recursive function 'calm' to replace exclamation marks in a char list with periods.
      Rewrite your function to use map instead of recursion. What are the types
      of your functions? *)
(* calm : char list -> char list *)
let rec calm char_list =
  match char_list with
  | [] -> []
  | h :: tl ->
    let elem = if h = '!' then '.' else h in
    elem :: calm tl

(* calm' : char list -> char list *)
let calm' = map (fun c -> if c = '!' then '.' else c)

(* 2. Write a function 'clip' which, given an integer, clips it to the range 1...10 so that integers bigger round down
      to 10, smaller round up to 1. Write another function cliplist which applies this function to alist of integers. *)
(* clip : int -> int *)
let clip x =
  if x < 1 then 1
  else if x > 10 then 10
  else x

(* cliplist : int list -> int list *)
let cliplist = map clip

(* 3. Write cliplist using an anonymous function *)
let cliplist' = map (fun x ->
                       if x < 1
                       then 1
                       else if x > 10 then 10
                       else x)

(* 4. Write a function, apply, which, given a function, a number of times to apply it, and an initial argument to the
      function, returns the cumulative effect of applying the function. What is the type of your function? *)

(* apply : ('a -> 'a) -> int -> 'a -> 'a *)
let rec apply f n acc =
  if n <= 0 then acc
  else apply f (n - 1) (f acc)

(* 5. Modify the insertion sort function from the previous chapter to take a comparison function. What is its type? *)

(* 6. Write a function 'filter' which takes a function from 'a -> bool and an 'a list and returns a list of just those
      elements for which the function returns true. *)

(* 7. Write the function 'for_all' which, given a function of type 'a -> bool and an argument list of type 'a list
      that evaluates to true only if the function returns tru for every element of the list. *)

(* 8. Write a function mapl which maps a function of type 'a -> 'b over a list of type 'a list list to produce a list
      of type 'b list list *)
