(* Definitions from the chapter *)

type 'a tree =
  | Br of 'a * 'a tree * 'a tree
  | Lf

(* tree_size : 'a tree -> int *)
(* the text calls this function simply `size` *)
let rec tree_size tr =
  match tr with
  | Lf -> 0
  | Br (_, l, r) -> 1 + (tree_size l) + (tree_size r)

(* tree_sum : int tree -> int *)
let rec tree_sum tr =
  match tr with
  | Lf -> 0
  | Br (e, l, r) -> e + (tree_sum l) + (tree_sum r)

(* max : 'a -> 'a -> 'a *)
let max x y = if x > y then x else y

(* max_depth : 'a tree -> int *)
let rec max_depth tr =
  match tr with
  | Lf -> 0
  | Br (_, l, r) -> 1 + max (max_depth l) (max_depth r)

(* list_of_tree : 'a tree -> 'a list *)
let rec list_of_tree tr =
  match tr with
  | Lf -> []
  | Br (e, l, r) -> list_of_tree l @ [e] @ list_of_tree r

(* tree_map : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec tree_map f tr =
  match tr with
  | Lf -> Lf
  | Br (e, l, r) -> Br (f e, tree_map f l, tree_map f r)

(* lookup_tree : ('a * 'b) tree -> 'a -> 'b option *)
let rec lookup_tree tr e =
  match tr with
  | Lf -> None
  | Br ((k, v), l, r) ->
    if e = k then Some v
    else if e < k then lookup_tree l e
    else lookup_tree r e

(* insert_tree : ('a * 'b) tree -> ('a * 'b) -> ('a * 'b) tree *)
let rec insert_tree tr (k, v) =
  match tr with
  | Lf -> Br ((k,v), Lf, Lf)
  | Br ((k',v'), l, r) ->
    if k = k' then Br ((k,v), l, r)
    else if k < k' then Br ((k',v'), insert_tree l (k,v), r)
    else Br ((k',v'), l, insert_tree r (k,v))


(* Questions *)
(* 1. Write a function of type `'a -> 'a tree -> bool` to determine if a given element is in a tree *)

(* this function has type ('a * 'b) tree -> 'a -> bool so this isn't what was asked for but it's nice  since it leverges
   the binary search properties of the tree *)
let contains_key tr k =
  match lookup_tree tr k with
  | None -> false
  | Some _ -> true

(* contains_element : 'a -> 'a tree -> bool *)
(* we aren't assuming anything about the structure of the tree (like left branches being smaller than right branches)
   so we check to see if the current element matches our searchign element otherwise we see if our element
   is in the left branch and if not we try the right branch. *)
let rec contains_element e tr =
  match tr with
  | Lf -> false
  | Br (e', l, r) ->
    if e = e' then true
    else contains_element e l || contains_element e r

(* 2. Write a function which flips a tree left to right such that, if it were drawn on paper,
      it would appear to be a mirror image. *)

let rec flip_tree tr =
  match tr with
  | Lf -> Lf
  | Br (e, l, r) -> Br (e, flip_tree r, flip_tree l)

(* 3. Write a function to determine if two trees have the same shape, irrespective of
      actual values of the elements. *)
(* isomorphic : 'a tree -> 'b tree -> bool *)
let rec isomorphic t1 t2 =
  match t1,t2 with
  | Lf, Lf -> true
  | Lf, Br (_) -> false
  | Br (_), Lf -> false
  | Br (_, lt1, rt1), Br (_, lt2, rt2) -> isomorphic lt1 lt2 && isomorphic rt1 rt2

(* 4. Write a function `tree_of_list` which builds a tree representation of a dictionary
      from a list representing a dictionary. *)

(* tree_of_list : ('a * 'b) list -> ('a * 'b) tree *)
let tree_of_list l =
  let rec tree_of_list' l t =
    match l with
    | [] -> t
    | (k,v) :: tl -> tree_of_list' tl (insert_tree t (k,v))
  in
  tree_of_list' l Lf

(* 5. Write a function to combine two dictionaries represented as trees into one.
      In the case of clashing keys, prefer the value from the first dictionary. *)

(* merge_trees : ('a * 'b) tree -> ('a * 'b) tree -> ('a *' b) tree *)
let merge_trees t1 t2 =
  let l1 = list_of_tree t1 in
  let l2 = list_of_tree t2 in
  tree_of_list (l2 @ l1)


(* 6. Can you define a type for trees which, instead of branching exactly two ways,
      can branch zero or more ways, possibly different at each branch? Write
      size, total, and map using your new tree type. *)

type 'a multi_tree =
  | Branch of 'a * 'a multi_tree list

(* sum : int list -> int *)
let rec sum l =
  match l with
  | [] -> 0
  | h :: tl -> h + sum tl

(* map : ('a -> 'b) -> 'a list -> 'b list *)
let rec map f l =
  match l with
  | [] -> []
  | h :: tl -> f h :: map f tl

(* multi_tree_size : 'a multi_tree -> int *)
let rec multi_tree_size (Branch (_, branches)) =
  1 + sum (map multi_tree_size branches)

(* multi_tree_total : int multi_tree -> int *)
let rec multi_tree_total (Branch (e, branches)) =
  e + sum (map multi_tree_total branches)

(* multi_tree_size : ('a ->' 'b) -> 'a multi_tree -> 'b multi_tree  *)
let rec multi_tree_map f (Branch (e, branches)) =
  Branch (f e, List.map (multi_tree_map f) branches)
