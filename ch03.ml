(* Definitions in the chapter *)

(* recall the factorial function defined in ch2 *)

let rec factorial_ch2 a =
  if a = 1 then 1 else a * factorial_ch2 (a - 1)

(* written with pattern matching: *)

let rec factorial_pat_match a =
  match a with
  | 1 -> 1
  | _ -> a * factorial_pat_match (a - 1)

(* recall the is_vowel function defined in ch2 *)

let is_vowel_ch2 c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

(* written with pattern matching *)

let is_vowel_pat_match c =
  match c with
  | 'a' -> true
  | 'e' -> true
  | 'i' -> true
  | 'o' -> true
  | 'u' -> true
  | _ -> false

let is_vowel_pat_match_short c =
  match c with
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false

let rec gcd_ch2 a b =
  if b = 0 then a else gcd_ch2 b (a mod b)

let rec gcd_pat_match a b =
  match b with
  | 0 -> a
  | _ -> gcd_pat_match b (a mod b)

(* Questions: *)
(* 1. Rewrite the not function from ch2 in pattern matching style *)
let not b =
  match b with
  | true -> false
  | false -> true

(* 2. Use pattern matching to write a recursive function which, given a positive
      integer n, returns the sum of the integers from 1 to n *)

let rec sum_from_1_to_n_pat_match n =
  if n < 1 then
    raise (Invalid_argument "Argument must be larger than 0")
  else
    match n with
    | 1 -> 1
    | _ -> n + sum_from_1_to_n_pat_match (n - 1)

(* 3. Use pattern matching to write a function which, given two numbers x and n,
      computes x^n *)

let rec pow_pat_match x n =
  if n < 0 then
    raise (Invalid_argument "Argument must be >= 0")
  else
    match n with
    | 1 -> 1
    | _ -> x * (pow_pat_match x (n - 1))

(* 4. For each of the previous three questions, do you find it easier to read
      the function with or without pattern matching? How might you expect this
      to change if the functions were much larger?

For these small functions I think it doesn't make much of a difference
in readability. For functions with more cases to match against I would
prefer pattern matching to long chains of if ... then ... else ... expressions.
*)

(* 5. What does match 1 + 1 with 2 -> match 2 + 2 with 3 -> 4 | 4 -> 5
      evaluate to? *)

(* evaluation:
match 1 + 1 with 2 -> match 2 + 2 with  3 -> 4 | 4 -> 5

match 2 with 2 -> match 4 with  3 -> 4 | 4 -> 5

match 4 with  3 -> 4 | 4 -> 5

5
*)

(* 6. There is a special pattern x..y to denote continuous ranges of characters,
      for example 'a'..'z' will match all lowercase letters. Write functions
      is_lower and is_upper, each of type char -> bool, to decide on the case
      of a given letter. *)

let is_lower c =
  match c with
  | 'a'..'z' -> true
  | _ -> false

let is_upper c =
  match c with
  | 'A'..'Z' -> true
  | _ -> false
