(* Definitions from the chapter *)

(* print_int : int -> unit *)
(* p_100 : unit -> unit *)
let p_100 () = print_int 100

(* print_dict_entry : (int * string) -> unit *)
let print_dict_entry (k,v) =
  print_int k;
  print_newline ();
  print_string v;
  print_newline ()

(* iter : ('a -> 'b) -> 'a list -> unit *)
let rec iter f l =
  match l with
  | [] -> ()
  | h :: tl -> f h ; iter f tl

(* print_dict : (int * string) list -> unit  *)
let print_dict = iter print_dict_entry

(* read_dict : unit -> (int * string) list *)
let rec read_dict () =
  try
    print_string "Please enter an integer";
    print_newline ();
    let i = read_int () in
    if i = 0 then [] else
      begin
        print_string "Please enter a name";
        print_newline ();
        let name = read_line () in
        (i, name) :: read_dict ()
      end
  with
    Failure _ ->
    print_string "That is not a valid value. Please try again.";
    print_newline ();
    read_dict ()

(* dictionary_to_file : string -> (int * string) list -> unit *)
let dictionary_to_file filename dict =
  (* these helper functions were previously defined outside of the scope of this function
     but since they're not relevant outside this scope I thought it best to keep them here *)
  (* dictionary_to_channel : out_channel -> (int * string) list -> unit *)
  let dictionary_to_channel ch d =
    (* entry_to_channel : out_channel -> (int * string) -> unit *)
    let entry_to_channel ch (k,v) =
      output_string ch (string_of_int k);
      output_char ch '\n';
      output_string ch v;
      output_char ch '\n'
    in
    iter (entry_to_channel ch) d
  in
  let ch = open_out filename in
  dictionary_to_channel ch dict;
  close_out ch

(* dictionary_of_file : string -> (int * string) list *)
let dictionary_of_file filename =
  (* these helper functions were previously defined outside of the scope of this function
     but since they're not relevant outside this scope I thought it best to keep them here *)
  (* entry_of_channel : in_channel -> (int * string) *)
  let entry_of_channel ch =
    let number = input_line ch in
    let name = input_line ch in
    (int_of_string number, name)
  in
  (* dictionary_of_channel : in_channel -> (int * string) list *)
  let rec dictionary_of_channel ch =
    try
      let entry = entry_of_channel ch in
      entry :: dictionary_of_channel ch
    with
      End_of_file -> []
  in
  let ch = open_in filename in
  let dict = dictionary_of_channel ch in
  close_in ch;
  dict

(* Questions *)
(* 1. Write a function to print a list of integers to the screen they way OCaml does (with square brackets and semicolons) *)

(* string_of_int_list : int list -> string *)
let string_of_int_list l =
  let buf = Buffer.create (List.length l) in
  Buffer.add_char buf '[';
  let rec list_to_buffer l buf =
    match l with
    | [] -> ()
    | [e] -> Buffer.add_string buf (string_of_int e);
    | h :: tl ->
      Buffer.add_string buf (string_of_int h);
      Buffer.add_char buf ';';
      list_to_buffer tl buf
  in
  list_to_buffer l buf;
  Buffer.add_char buf ']';
  Buffer.contents buf

(* print_int_list : int list -> unit *)
let print_int_list l = print_string (string_of_int_list l)

(* 2. Write a function to read three integers from the user and return them as a tuple.
      Be sure to handle exceptions that could be raised. *)

(* read_triple : unit -> (int * int * int) *)
let read_triple () =
  let rec read_int_from_user () =
    print_string "Please enter an integer:";
    print_newline ();
    try read_int ()
    with Failure _ ->
      print_string "That value was invalid. Please enter an integer.";
      print_newline ();
      read_int_from_user ()
  in
  let first = read_int_from_user () in
  let second = read_int_from_user () in
  let third = read_int_from_user () in
  (first,second,third)


(* 3. In our read_dict function, we waited for the user to type 0 to indicate no more data.
      Thisis clumsy. Implement a new read_dict function with a nicer system. Be careful to deal with exceptions. *)

(* read_dict_from_user : unit -> (int * string) list *)
let rec read_dict_from_user () =
  let read_entry () =
    let rec read_key () =
      print_string "Please enter an integer or enter 'done' if you're done.";
      print_newline ();
      let l = read_line () in
      if l = "done" then
        None
      else
        try
          Some (int_of_string l)
        with
          Failure _ ->
          print_string "That value was invalid. Please enter an integer or enter 'done'.";
          print_newline ();
          read_key ()
    in
    let read_value () =
      print_string "Please enter a value";
      print_newline ();
      read_line ()
    in
    match (read_key ()) with
    | None -> None
    | Some k -> Some (k, read_value ())
  in
  match (read_entry ()) with
  | None -> []
  | Some e -> e :: read_dict_from_user ()

(* 4. Write a function which, given a number, x, prints the x by x times table to a given filename. *)
(* print_times_table : int -> unit *)
let print_times_table x =
  for row = 1 to x do
    for column = 1 to x do
      print_int (row * column);
      print_char '\t';
    done;
    print_newline ();
  done

(* 5. Write a function to count the number of lines in a given file. *)
(* wc : string -> int *)
let wc filename =
  let ch = open_in filename in
  let rec wc' ch count =
    try
      let _ = input_line ch in
      wc' ch (1 + count)
    with
      End_of_file -> count
  in
  wc' ch 0

(* 6. Write a function copy_file of type string -> string -> unit which copies a file
      line by line. Make sure to handle the case where the source file cannot be found
      or where the destination file cannot be created or filled. *)

(* copy : string -> string -> unit *)
let copy source dest =
  let source_ch = open_in source in
  let dest_ch = open_out dest in
  let rec copy_ch source_chan dest_chan =
    try
      let line = input_line source_chan in
      output_string dest_chan line;
      output_char dest_chan '\n';
      copy_ch source_chan dest_chan
    with
    | End_of_file -> ()
    | Sys_error e ->
      print_string "Unable to copy file";
      print_newline ();
      print_string e;
      print_newline ()
  in
  copy_ch source_ch dest_ch;
  close_in source_ch;
  close_out dest_ch
