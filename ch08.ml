(* Definitions from the chapter *)

(* p : (int * int ) *)
let p = (1, 4)

(* q : (int * char) *)
let q = (1, '1')

(* fst : ('a * 'b) -> 'a *)
let fst (x, _) = x

(* snd : ('a * 'b) -> 'b *)
let snd (_, y) = y

(* census : (int * int) list *)
let census = [(1, 4); (2 , 2); (3, 2); (4, 3); (5, 1); (6, 2)]

(* y : (int * int list) *)
let y = (1, [2; 3; 4])

(* lookup : 'a -> ('a * 'b) list -> 'b option *)
(* slightly different than the book implemenation which raises Not_found *)
let rec lookup x l =
  match l with
  | [] -> None
  | (k, v) :: tl ->
    if k = x then Some v else lookup x tl

(* add : ('a * 'b) -> ('a * 'b) list -> ('a * 'b) list *)
(* slightly different than the book version which accepts k and v as separate arguments
   instead of together as a pair *)
let rec add (k, v) l =
  match l with
  | [] -> [(k, v)]
  | (k', v') :: tl ->
    if k = k' then (k, v) :: tl
    else (k', v') :: add (k,v) tl

(* remove : 'a -> ('a * 'b) list -> ('a * 'b) list *)
let rec remove k l =
  match l with
  | [] -> []
  | (k', v') :: tl ->
    if k = k' then tl else (k', v') :: remove k tl

(* key_exists : 'a -> ('a * 'b) list -> bool *)
(* slightly different from the text implmentation since the implementation of lookup above
   uses an option type instead of exceptions. *)
let key_exists k l =
  match lookup k l with
  | None -> false
  | Some _ -> true

(* Questions *)

(* 1. Write a function to determine the number of different keys in a dictionary *)
(* assuming all keys are unique which seems acceptable since the add function respects that *)

(* count_keys : ('a * 'b) list -> int *)
let rec count_keys l =
  match l with
  | [] -> 0
  | (_,_) :: tl -> 1 + count_keys tl

(* 2. Define a function replace which is like add, but raises Not_found if the key is not already there. *)

(* replace : ('a * 'b) -> ('a * 'b) list -> ('a * 'b) list *)
let replace (k,v) l =
  if key_exists k l then add (k,v) l
  else (raise Not_found)


(* 3. Write a function to build a dictionary from two equal length lists, one containing keys
      and anotehr containing values. Raise the exception Invalid_argument if the lists are not of equal length. *)

(* build_dictionary : 'a list -> 'b list -> ('a * 'b) list *)
let rec build_dictionary keys values =
  if List.length keys != List.length values
  then raise (Invalid_argument "keys and values must be the same length")
  else
    match keys, values with
    | [],[]
    | (_ :: _), []
    | [], (_ :: _) -> []
    | hk :: tlk, hv :: tlv -> (hk,hv) :: build_dictionary tlk tlv

(* the exception seems silly since we need to handle the mismatched size cases anyway in order
   to avoid a warning about inexhaustive pattern matches *)

(* build_dictionary' : 'a list -> 'b list -> ('a * 'b) list *)
let rec build_dictionary' keys values =
  match keys, values with
    | [],[]
    | (_ :: _), []
    | [], (_ :: _) -> []
    | hk :: tlk, hv :: tlv -> (hk,hv) :: build_dictionary' tlk tlv

(* 4. Now write the inverse function. Given a dictionary return the pair of two lists -
      the first containing the keys, the second containing the values. *)

(* deconstruct : ('a * 'b) list -> ('a list * 'b list) *)
let deconstruct d =
  let rec deconstruct' d keys values =
    match d with
    | [] -> (keys, values)
    | (k,v) :: tl -> deconstruct' tl (k :: keys) (v :: values)
  in
  deconstruct' d [] []

(* 5. Define a function to turn any list of pairs into a dictionary. If duplicate keys are fund, the value associated
      with the first occurrence should be kept. *)

(* add_no_replace : ('a * 'b) -> ('a * 'b) list -> ('a * 'b) list *)
let rec add_no_replace (k,v) d =
  match d with
  | [] -> [(k,v)]
  | (k',v') :: tl ->
    if k = k' then (k',v') :: tl
    else (k',v') :: add_no_replace (k,v) tl

(* construct : ('a * 'b) list -> ('a * 'b) list *)
let construct l =
  let rec construct' l d =
    match l with
    | [] -> d
    | (k,v) :: tl -> construct' tl (add_no_replace (k,v) d)
  in
  construct' l []

(* 6. Write a function which forms the untion of two dictionaries. The union of two
      dictionaries is the dictionary containing all the entries either dictionary.
      In the case that a key is in both dictionaries, the value in the first should
      be preferred. *)

(* union : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list *)
let union a b = construct (a @ b)
