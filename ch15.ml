(* Questions *)
(* 1. Write your own tail-recursive version of the function List.concat. *)

let concat ll =
  let rec concat' ll acc =
    match ll with
    | [] -> List.rev acc
    | h :: tl -> concat' tl (List.rev_append h acc)
  in
  concat' ll []

(* 2. Use List.mem to write a function which returns true only if every list in
   a `bool list list` contains true somewhere in it. *)

let every_list_has_a_true ll = List.for_all (fun l -> List.mem true l) ll

(* or if we want to abbreviate the definition using partial application *)
let every_list_has_a_true_partial_app = List.for_all (List.mem true)

(* 3. Write a function to count the number of exclamation marks in a string,
   using one or more functions from the String module. *)

let number_of_bangs s =
  let count = ref 0 in
  String.iter (fun c -> if c = '!' then count := !count + 1;) s;
  !count

(* 4. Use the String.map function to write a function to return a new copy
   of the string with all exclamation marks replaced with periods. *)

let enhance_your_calm = String.map (fun c -> if c = '!' then '.' else c)

(* 5. Use the String module to write a function which concatenates a list of
   strings together. *)

let concat_strings = String.concat " "

(* 6. Do the same with the Buffer module. This will be faster. *)

let concat_strings_buffer l =
  if List.length l < 1 then
    ""
  else
    let buf = Buffer.create 16 in
    let rec concat_strings' l buf =
      match l with
      | [] -> Buffer.contents buf (* this case can't really happen because we don't
                                     even allocate the buffer if the list is empty.
                                     but the type system doesn't know that :p *)
      | [e] ->
        begin
          (Buffer.add_string buf e);
          Buffer.contents buf
        end
      | h :: tl ->
        begin
          (Buffer.add_string buf h);
          (Buffer.add_char buf ' ');
          concat_strings' tl buf
        end
    in
    concat_strings' l buf

(* 7. Use the String module to count the number of occurrences of the string
   "OCaml" within a given string. *)

let occurrences term string =
  let rec occurrences' term string count =
    if string = "" || String.length term > String.length string then count
    else
      let candidate = String.sub string 0 (String.length term) in
      if candidate = term then
        let rest = (String.sub string (String.length term) (String.length string - (String.length term))) in
        occurrences' term rest (count + 1)
      else
        let rest = (String.sub string 1 (String.length string - 1)) in
        occurrences' term rest count
  in
  occurrences' term string 0
