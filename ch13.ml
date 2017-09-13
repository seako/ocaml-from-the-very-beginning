(* Definitions from the chapter *)

(* x : int ref *)
let x = ref 0

(* p : int *)
let p = !x;;

(* updating the contents of a ref returns unit, not the value of the update *)
x := 50;;

(* q : int *)
(* p is unchanged *)
let q = !x;;

(* swap : a' ref -> a' ref -> unit *)
let swap a b =
  let a_v = !a in
  a := !b;
  b := a_v

(* print_nats : int -> unit *)
let print_nats n =
  if n > 0 then
    for i = 1 to n do
      print_int i;
      print_newline ()
    done

(* smallest_pow2_gteq_n_imperative : int -> int *)
let smallest_pow2_gteq_n_imperative n =
  let x = ref 1 in
  while !x < n do
    x := !x * 2
  done;
  !x

(* smallest_pow2_gteq_n_functional : int -> int *)
let smallest_pow2_gteq_n_functional n =
  let rec smallest_pow2_gteq_n x =
    if x < n then smallest_pow2_gteq_n (x * 2)
    else x
  in
  smallest_pow2_gteq_n 1

type file_statistics =
  {line_count : int;
   char_count : int;
   space_count : int;
   sentence_count : int;
   histogram : int array}

type file_statistics_no_histogram =
  {line_count : int;
   char_count : int;
   space_count : int;
   sentence_count : int;}

(* collect_statistics_imperative : in_channel -> file_statistics *)
let collect_statistics_imperative in_channel =
  let line_count = ref 0 in
  let char_count = ref 0 in
  let space_count = ref 0 in
  let sentence_count = ref 0 in
  let histogram = Array.make 256 0 in
  let end_of_file = ref false in
  while (not !end_of_file) do
    try
      let line = input_line in_channel in
      line_count := !line_count + 1;
      char_count := !char_count + String.length line;
      String.iter
        (function
          | '.' | '?' | '!' -> sentence_count := !sentence_count + 1
          | ' ' -> space_count := !space_count + 1
          | _ -> ())
        line;
      String.iter
        (fun c ->
           let i = int_of_char c in
           histogram.(i) <- histogram.(i) +1 )
        line
    with
    | End_of_file -> end_of_file := true
  done;
  {line_count = !line_count;
   char_count = !char_count;
   space_count = !space_count;
   sentence_count = !sentence_count;
   histogram}

(* I think the reason that something like this function isn't included in the
   standard library is because strings used to be mutable?
   string_reduce : ('a -> char -> 'a) -> 'a -> string -> 'a *)
let string_reduce f init s =
  let acc = ref init in
  let f' c = acc:= f !acc c in
  String.iter f' s;
  !acc

(* count_spaces : string -> int *)
let count_spaces s =
  string_reduce
    (fun acc c ->
       match c with
       | ' ' -> acc + 1
       | _ -> acc)
    0
    s

(* count_sentences : string -> int *)
let count_sentences s =
  string_reduce
    (fun acc c ->
       match c with
       | '.' | '?' | '!' -> acc + 1
       | _ -> acc)
    0
    s

(* collect_statistics_functional_no_hist : in_channel -> file_statistics_no_histogram *)
let collect_statistics_functional_no_hist in_channel =
  let rec wc_functional' statistics in_channel =
    try
      match statistics with {line_count;
                             char_count;
                             space_count;
                             sentence_count} ->
        let line = input_line in_channel in
        let chars = String.length line in
        let spaces = count_spaces line in
        let sentences = count_sentences line in
        wc_functional' {line_count = (line_count + 1);
                        char_count = (char_count + chars);
                        space_count = (space_count + spaces);
                        sentence_count = (sentence_count + sentences)} in_channel
    with
    | End_of_file -> statistics
  in
  wc_functional' {line_count = 0;
                  char_count = 0;
                  space_count = 0;
                  sentence_count = 0} in_channel

let collect_statistics in_channel =
  let histogram = Array.make 256 0 in
  let eof = ref false in
  while (not !eof) do
    try
      let char = input_char in_channel in
      let i = int_of_char char in
      histogram.(i) <- histogram.(i) + 1;
    with End_of_file -> eof := true;
  done;
  histogram

(* print_statistics : file_statistics -> unit *)
let print_statistics {line_count; space_count; char_count; sentence_count} =
  print_string "There were ";
  print_int line_count;
  print_string " lines, ";
  print_int space_count;
  print_string " spaces, ";
  print_int char_count;
  print_string " characters, and ";
  print_int sentence_count;
  print_string " sentences";
  print_newline ()

(* print_character_histogram : int arry -> unit *)
let print_character_histogram histogram =
  print_string "character frequencies: ";
  print_newline ();
  for x = 0 to 255 do
    if histogram.(x) > 0 then
      begin
        print_string "character '";
        print_char (char_of_int x);
        print_string "' (character number ";
        print_int x;
        print_string ") the count is ";
        print_int histogram.(x);
        print_string ".";
        print_newline ()
      end
  done

(* print_channel_statistics : in_channel -> unit *)
(* I split out this function into a function for collecting data and a function
   for printing the collected data as a report. Because intermingling the two
   makes me grumpy. *)
let print_channel_statistics in_channel =
  let {line_count;
       char_count;
       space_count;
       sentence_count;
       histogram} = collect_statistics_imperative in_channel in
  let collected_imperative_statistics_no_hist = {line_count;
                                                 char_count;
                                                 space_count;
                                                 sentence_count} in
  seek_in in_channel 0;
  let collected_functional_statistics_no_hist =
    collect_statistics_functional_no_hist in_channel in


  print_string "imperative statistics: ";
  print_newline ();
  print_statistics collected_imperative_statistics_no_hist;
  print_character_histogram histogram;
  print_string "functional statistics: ";
  print_newline ();
  print_statistics collected_functional_statistics_no_hist


(* print_file_statistics : string -> unit *)
let print_file_statistics filename =
  let chan = open_in filename in
  try
    print_channel_statistics chan;
    close_in chan;
  with
  | _ -> close_in chan

let histogram_report filename =
  let ch = open_in filename in
  let hist = collect_statistics ch in
  print_character_histogram hist;
  close_in ch

let file_statistics_of_histogram hist =
  let newline_code = int_of_char '\n' in
  let space_code = int_of_char ' ' in
  let period_code = int_of_char '.' in
  let question_mark_code = int_of_char '?' in
  let exclamation_point_code = int_of_char '!' in

  let line_count = hist.(newline_code) in
  let space_count = hist.(space_code) in
  let char_count = (Array.fold_left (+) 0 hist) - line_count in
  let sentence_count = hist.(period_code) +
                       hist.(question_mark_code) +
                       hist.(exclamation_point_code) in
  {line_count; space_count; char_count; sentence_count}

let collect_print_stats filename =
  let ch = open_in filename in
  let hist = collect_statistics ch in
  let stats = file_statistics_of_histogram hist in
  print_statistics stats;
  print_character_histogram hist

(* Questions *)
(* 1. Consider the following expression:
   let x = ref 1 in let y = ref 2 in x := !x + !x; y := !x + !y; !x + !y
   What references have been created? What are their initial and final values?
   What is the type of this expression?

   Two references `x` and `y` have been created. `x` is initialized to 1,
   `y` is initialized to 2. Their final values are 2 and 4 respectively.
   The type of this expression is int because the the final return value is
   the result of the sum of the contents of the refts `x` and `y`.
*)

(* 2. What is the difference between `[ref 5; ref 5]` and
   `let x = ref 5 in [x ; x]`?

   [ref 5; ref 5] is a list of two refs containing the value 5
   let x = ref 5 in [x ; x] is a list containing two `x`s, a ref containing
   the value 5. In the latter case updating the ref `x` updates both elements
   of the list whereas in the former case, the two refs must be updated
   independently. *)

(* 3. Imagine that the for...to...do...done construct did not exist.
   How might we create the same behavior? *)

(* loop : int -> int -> (unit -> 'a) -> unit *)
let rec loop initial terminal f =
  if initial > terminal
  then ()
  else
    begin
      f ();
      loop (initial + 1) terminal f
    end

(* loop' : int -> int -> (unit -> 'a) -> unit *)
let loop' initial terminal f =
  if initial < terminal then
    let i = ref initial in
    while !i <= terminal do
      f ();
      i := !i + 1
    done

(* 4. What are the types of these expressions?
   [|1;2;3|] is of type `int array`
   [|true; false; true|] is of type `bool array`
   [|[|1|]|] is of type `int array array`
   [|[|1; 2; 3|]; [|4; 5; 6|]|] is of type `int array array`
   [|1; 2; 3|].(2) is of type `int`
   [|1; 2; 3|].(2) <- 4 is of type `unit`*)

(* 5. Write a function to compute the sum of the elements in an integer array *)

(* iterating through the array with a loop *)
let array_sum_loop a =
  let sum = ref 0 in
  for i = 0 to (Array.length a) - 1 do
    sum := !sum + a.(i);
  done;
  !sum

(* summing the elements with a fold *)
let array_sum_fold a = Array.fold_left (+) 0 a

(* 6. Write a function to reverse the elements of an array in place
   (do not create a new array )*)

(* array_reverse : 'a array -> 'a array *)
let array_reverse a =
  let swap_elements a left_index right_index =
    let left = a.(left_index) in
    a.(left_index) <- a.(right_index);
    a.(right_index) <- left
  in
  let rec array_reverse' a left_index right_index =
    if left_index >= right_index
    then a
    else
      begin
        swap_elements a left_index right_index;
        array_reverse' a (left_index + 1) (right_index - 1)
      end
  in
  array_reverse' a 0 (Array.length a - 1)

(* 7. Write a function `table` which, given an integer, builds the
   `int array array` representing the multiplication table up to that number. *)

let table n =
  let matrix = Array.make_matrix n n 0 in
  for i = 1 to n do
    for j = 1 to n do
      matrix.(i - 1).(j - 1) <- i * j;
    done;
  done;
  matrix

(* 8. Use the built-in functions int_of_char and char_of_int to write functions
   to uppercase and lowercase a letter. Non-alphabetic characters should remain
   unaltered. *)

let uppercase char =
  match char with
  | 'a'..'z' -> char_of_int ((int_of_char char) + 32)
  | _ -> char

let lowercase char =
  match char with
  | 'A'..'Z' -> char_of_int ((int_of_char char) - 32)
  | _ -> char

(* 9. Comment on the accuracy of our character, word, line and sentence
   statistics.

   The character count presented in the text does not count line numbers.
   The word count really just counts spaces and would regard the string "   "
   as consisting of three words. Line count is fine. Sentence count can over count
   sentences. For example, it would regard the single sentence consisting of the
   exclamation, "What?!?!?", as 5 sentences. *)

(* 10. Choose one of the problems you have identified and modify the program
   to fix it.

   Above I made a number of modifications to try to calculate the sentence
   statistics to my liking. I mainly focused on splitting data collection from
   report generation and report presentation but I also fixed the problem of the
   character count not including newline characters. see `collect_print_stats` *)
