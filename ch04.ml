(* Definitions in the chapter *)

(* a_list : int list *)
let a_list = [1; 2; 3]

(* an_empty_list : 'a list '*)
let an_empty_list = []

(* bool_list : bool list *)
let bool_list = [false; true; false]

(* another_bool_list : bool list *)
let another_bool_list = false :: [true ; false]

(* appended_int_lists : int list *)
let appended_bool_lists = [1 ; 2] @ [3 ; 4; 5]

(* the book calls this function isnil
is_empty : 'a list -> bool
*)

let is_empty l =
  match l with
  | [] -> true
  | _ -> false


(* length : 'a list -> int *)
let rec length l =
  match l with
  | [] -> 0
  | h::tl -> 1 + length tl

(* but we don't really need to name the head because we don't use it *)
(* headless_length : 'a list -> int *)
let rec headless_length l =
  match l with
  | [] -> 0
  | _::t -> 1 + headless_length t

(* sum : int list -> int *)
let rec sum l =
  match l with
  | [] -> 0
  | h::tl -> h + sum tl

(* the book calls this just 'length' *)
(* length_accum : 'a list -> int *)
let length_using_accumulator l =
  (* the book calls this length_inner *)
  (* length_aux : 'a list -> int -> int *)
  let rec length_aux l' acc =
    match l' with
    | [] -> acc
    | _::tl -> length_aux tl (1 + acc)
  in
  length_aux l 0


(* odd_elements : 'a list -> 'a list *)
let rec odd_elements l =
  match l with
  | [] -> []
  | [e] -> [e]
  | h :: _ :: tl -> h :: odd_elements tl

(* odd_elements_fewer_cases : 'a list -> 'a list *)
(* this function has fewer pattern matches but I prefer the version
   that makes all the cases explicit. *)
let rec odd_elements_fewer_cases l =
  match l with
  | h :: _ :: tl -> h :: odd_elements_fewer_cases tl
  | _ -> l

(* our_append : 'a list -> 'a list -> 'a list *)
(* the book just calls this 'append' *)
let rec our_append l r =
  match l with
  | [] -> r
  | h :: tl -> h :: our_append tl r

(* our_rev_bad : 'a list -> 'a list *)
let rec our_rev_bad l =
  match l with
  | [] -> []
  | h :: tl -> our_rev_bad tl @ [h]

(* our_rev_better : 'a list -> 'a list *)
let our_rev_better l =
  let rec rev_aux input output =
    match input with
    | [] -> output
    | h :: tl -> rev_aux tl (h :: output)
  in
  rev_aux l []

(* our_take : 'a list -> int -> 'a list *)
let rec our_take n l =
  if n <= 0 then [] else
    match l with
    | [] -> []
    | h :: tl -> h :: our_take (n - 1) tl

let rec our_drop n l =
  if n <= 0 then l else
    match l with
    | [] -> []
    | _ :: tl -> our_drop (n - 1) tl

(* Questions: *)

(* 1. Write a function which returns the even numbered elements in a list.
      What is the type of your function? *)

(* even_elements : 'a list -> 'a list *)
let rec even_elements l =
  match l with
  | [] -> []
  | [_] -> []
  | _ :: cdadr :: tl -> cdadr :: even_elements tl

(* 2. Write a function count_true which counts the number of true elements in a list.
      What is the type of your function? Can you write a tail recursive version? *)

(* count_true : bool list -> int *)
(* this function is tail recursive *)
let count_true l =
  let rec count_true_aux l' acc =
    match l' with
    | [] -> acc
    | h :: tl -> count_true_aux tl (if h then (acc + 1) else acc)
  in
  count_true_aux l 0

(* 3. Write a function which, given a list builds a palindrome from it. A
      palindrome is a list which equals its own reverse. You can assume
      The existence of rev and @. Write another function which determines
      if a list is a palindrome. *)

let palindrome_of_list l =
  l @ our_rev_better l

let is_palindrome l =
  l = our_rev_better l

(* 4. Write a function drop_last which returns all but the last element of
      a list. If the list is empty, itshould return the empty list.
      What about a tail recursive version? *)

let rec drop_last l =
  match l with
  | [] -> []
  | [_] -> []
  | h :: tl -> h :: drop_last tl

let drop_last_tail_rec l =
  let rec drop_last_aux input output =
    match input with
    | [] -> output
    | [_] -> output
    | h :: tl -> drop_last_aux tl (h :: output)
  in
  our_rev_better (drop_last_aux l [])

(* 5. Write a function member of type 'a -> 'a list -> bool which returns true
      if an element exists in a list, or false if not. *)

let rec member e l =
  match l with
  | [] -> false
  | h :: tl -> if e = h then true else member e tl

(* 6. Use the member function to write make_set. Make set takes a list and returns
      a list with no duplicate elements. *)

(* make_set : 'a list -> 'a list *)
let make_set l =
  let rec make_set_aux input output =
    match input with
    | [] -> output
    | h :: tl ->
      if member h output
      then make_set_aux tl output
      else make_set_aux tl (h :: output)
  in
  make_set_aux l []

(* 7. Can you explain why the rev (our_rev_bad) function we defined is inefficient?
      How does the time it takes to run relate to the size of its argument? Can you
      write a more efficient verion using an accumulating argument? What is its
      efficiency in terms of time taken and space used?

For a list of length n, our_rev_bad builds up n calls to @. Each of the invocations
Of @ will operate on a list of length 1, 2, 3, ..., n -1. So our_rev_bad is takes
time proportionate to the factorial of the length of its input.

our_rev_better uses an accumulator and takes time proportional to the length of its input *)
