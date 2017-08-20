(* Definitions in the chapter *)

(* Naming the result of expressions *)

let x = 200;;

let y = 200 in y * y * y;;

let a = 500 in (let b = a * a in a + b);;

(* Defining named functions*)

let cube x = x * x * x

let is_neg x = if x < 0 then true else false

let is_vowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o'|| c = 'u'

let add_ten i = i + 10

let rec factorial x =
  if x = 1 then 1 else x * factorial (x - 1)

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let not x = if x then false else true

(* Questions: *)

(* 1. Write a function which multiplies a given number by ten. What is its type? *)

let mult_by_ten x = x * 10

(* this function has type int -> int *)

(* 2. Write a function which returns true if both of its arguments are non-zero and false otherwise.
      What is the type of this function? *)

let both_non_zero a b =
  if (a != 0) && (b != 0) then true else false

(* this function has type int -> int -> bool *)

(* 3. Write a recursive function which, given a number n, calculates the sum 1 + 2 + 3 + ... + n.
      What is its type? *)

let rec sum_from_one_to_n n =
  if n <= 1 then n
  else n + sum_from_one_to_n (n - 1)

let sum_from_one_to_n_closed_form n = (n * (n + 1)) / 2

(* 4. Write a function power x n which raises x to the power n. Give its type. *)

let rec power x n =
  if n <= 0 then 1 else x * (power x (n - 1))

(* this function has type int -> int -> int *)

(* Write a function is consonant which, given a lower-case character in the range 'a'...'z',
   determines if it is a consonant. *)

let is_consonant c =
  if (c >= 'a' && c <= 'z') then not (is_vowel c) else false

(* the question didn't ask but this function has the type char -> bool *)

(* 6. What is the result of the expression let x = 1 in let x = 2 in x + x ? *)

(* this expression evaluates to 4 since the inner x is in scope for the expression x + x  *)

(* 7. Can you suggest a way of preventing the non-termination of the factorial function
      in the case of a zero or negative argument? *)

(* You could raise an exception that the argument is invalid *)

let rec factorial_that_raises x =
  if x < 1 then raise (Invalid_argument "Argument should be positive")
  else
  if x = 1 then x else x * factorial_that_raises (x -1)

(* Or you could just return 1 if the value is <= 1 *)

let rec factorial_that_just_returns_one_for_invalid_arg x =
  if x <= 1 then 1 else
    x * factorial_that_just_returns_one_for_invalid_arg (x - 1)
