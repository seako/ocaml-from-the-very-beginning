(* Definitions from the chapter *)

(* add : int -> int -> int *)
let add x y = x + y

(* map : ('a -> 'b) -> 'a list -> 'b list *)
let rec map f l =
  match l with
    | [] -> []
    | h :: tl -> (f h) :: map f tl

(* map_mult_by_two : int list -> int list *)
let map_mult_by_two = map (fun x -> x * 2)

(* map_mult_by_two' : int list -> int list *)
let map_mult_by_two' = map (( * ) 2)

(* mapl : ('a -> 'b) -> 'a list list -> 'b list list *)
let rec mapl f l =
  match l with
  | [] -> []
  | h :: tl -> map f h :: mapl f tl

(* or this implementation, which uses partial application *)
(* mapl' : ('a -> 'b) -> 'a list list -> 'b list list *)
let mapl f l = map (map f) l

(* or this implementation, which uses even more partial application *)
(* mapl'' : ('a -> 'b) -> 'a list list -> 'b list list *)
let mapl f = map (map f)

(* making the function structure explicit *)
(* add' : int -> int -> int *)
let add' = fun x -> fun y -> x + y

(* Questions *)
(* 1. Rewrite the chapter summary paragraph about the structure of f x y for a function of three arguments, g a b c.
      The function g a b c has type 'a -> 'b -> 'c -> 'd. It takes an argument of type 'a and returns a function of type
      'b -> 'c -> 'd. When given an argument of type 'b it returns a function from 'c -> 'd
       which returns a value of type 'd when given a value of type 'c.
       let g a b c = ... is a short hand for let g = fun a -> fun b -> fun c -> ... .*)

(* 2. Recall the function member x l which determines if an element x is contained in a list l.
      What is its type? Use partial application to write a function member_all x ls which
      determines if an element is a member of all the lists in ls. *)

(* member : 'a -> 'a list -> bool *)
let rec member x l =
  match l with
  | [] -> false
  | h :: tl -> if h = x then true else member x tl

(* every : ('a -> bool) -> 'a list -> bool *)
let rec every pred l =
  match l with
  | [] -> true
  | h :: tl -> (pred h) && every pred tl

(* member_all : 'a -> 'a list list -> bool *)
let member_all x = every (member x)

(* 3. Why does the function `map ((/) 2) [10;20;30]` not halve all the elements of the list? Write a function that would.

   That function doesn't halve all the elements of the list because the first argument to (/) is the dividend and not the divisor. *)

(* divide : int -> int -> int *)
let divide divisor dividend = dividend / divisor

(*: map_divide_by : int -> int list -> int list *)
let map_divide_by n = map (divide n)

(* 4. Write a function mapll which maps a function over lists of lists of lists.
      Do not use let rec. Is it possible to write a function which works like map, mapl
      or mapll depending on the list given to it? *)

let mapll f = map (map (map f))

(* Now the text says that you can't do this becuase an ocaml function can only have a single type. But, if you define a
   a new datatype that has single list, list of list and list of list of list constructors, then you can do it since you
  can destructure the constructors and return the correct constructor.
  Having to update the constructors and all their use sites would be pretty tedious if
  that was a thing you needed to do often. But, while kind of a hack, this does meet the requirements of working like
  map, mapl, or mapll depending on the list given to it. *)

type 'a nested_list =
  | Single of 'a list
  | Double of 'a list list
  | Triple of 'a list list list

(*: map_star : ('a -> b) -> 'a nested_list -> 'b nested_list *)
let map_star f nl =
  match nl with
  | Single s -> Single (map f s)
  | Double d -> Double (mapl f d)
  | Triple t -> Triple (mapll f t)

(* 5. Write a function `truncate` which takes an integer and a list of lists and returns
      a list of lists, each of which has been truncated to the given length. If a list
      is shorter than the given length, it is unchanged. Use partial application. *)

(* take : int -> 'a list -> 'a list *)
let rec take n l =
  if n <= 0 then []
  else match l with
    | [] -> []
    | h :: tl -> h :: take (n - 1) tl

(* truncate : int -> 'a list list -> 'a list list *)
let truncate n = map (take n)

(* 6. Write a function which takes a list of integers and retuns the list composed of
      all teh first elements of the lists. If a list is empty, a given number should be
      used in place of the first element. *)

(* first_or_zero : int list -> int *)
let first_or_zero l =
  match l with
  | [] -> 0
  | h :: _ -> h

(* this is what was asked for: return the first element or some default of each list *)
(* first_ints : int list -> int list *)
let first_ints = map first_or_zero

(* first : 'a list -> 'a option *)
let first l =
  match l with
  | [] -> None
  | h :: _ -> Some h

(* but this is a bit better since it works for any type and doesn't require a silly default *)
(* firsts : 'a list list -> 'a option list *)
let firsts = map first
