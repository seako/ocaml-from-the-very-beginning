(* Definitions from the chapter *)

(* these implementations are slightly different from those in the text since they
   are named with the suffix '_exn' and perform argument checking once rather than
   in each arm of the case match *)

(* take_exn : int -> 'a list -> 'a list *)
let rec take_exn n l =
  if n < 0 then raise (Invalid_argument "The numeric argument to take must be positive")
  else
    match l with
    | [] -> []
    | h :: tl -> if n = 0 then [] else h :: take_exn (n - 1) tl

(* drop_exn : int -> 'a list -> a' list *)
let rec drop_exn n l =
  if n < 0 then raise (Invalid_argument "The numeric argument to drop must be positive")
  else
    match l with
    | [] -> []
    | h :: tl -> if n = 0 then (h :: tl) else drop_exn (n - 1) tl

(* safe_divide : int -> int -> int *)
let safe_divide x y =
  try x / y with
    Division_by_zero -> 0

(* last_exn : 'a list -> 'a *)
let rec last_exn l =
  match l with
  | [] -> raise Not_found
  | [x] -> x
  | _ :: tl -> last_exn tl

(* not in the text but we could also make this function return an 'a option indicating that
   there might not be a last element (as in the case of an empty list) *)
(* last_option : 'a list -> 'a option *)
let rec last_option l =
  match l with
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last_option tl

(* Questions *)
(* 1. Write a function 'smallest' which returns the smallest positive element of a list of integers. If there is no
      positive element, it should raise Not_found. *)
(* smallest : int list -> int *)
let smallest l =
  let rec smallest' l min =
    match l with
    | [] -> min
    | h :: tl -> smallest' tl (if h < min then h else min)
  in
  let positive_elements = List.filter (fun x -> x >= 0) l in
  match positive_elements with
  | [] -> raise Not_found
  | h :: tl -> smallest' tl h

(* 2. Write a function 'smallest_or_zero' which uses returns zero if smallest raises Not_found. *)

(* smallest_or_zero : int list -> int *)
let smallest_or_zero l =
  try smallest l
  with Not_found -> 0

(* 3. Write an exception definition and a function which calculates the largest integer smaller than or equal to
      the square root of a given integer. If the argument is negative, the exception should be raised.*)
exception Argument_is_negative

let largest_integer_smaller_than_or_equal_to_sqrt_n n =
  if n < 0
  then (raise Argument_is_negative)
  else int_of_float (sqrt (float_of_int n))

(* 4. Write another function that uses the previous one, but handles the exception and simply returns zero when
      a suitable integer cannot be found. *)

let largest_integer_smaller_than_or_equal_to_sqrt_n_or_zero n =
  try largest_integer_smaller_than_or_equal_to_sqrt_n n
  with Argument_is_negative -> 0

(* 5. Comment on the merits and demerits of exceptions as a method for dealing with exceptionsal situations, in contrast
      to returning a special value.
Exceptions seem better than special values like -1. Since they make it clear that something errant happened.
But they seem worse than mechanisms like Result or Option types since those allow the type system to help you ensure
those errant values are handled appropriately, whereas exceptions can go unhandled. *)
