(* 2014-17189 이소희
 * Exercise 1-1, Due: 9/14, 24:00 *)

let rec merge ((ilist1 : int list), (ilist2 : int list)) : int list =
  match (ilist1, ilist2) with
  |([], []) -> []
  |([], ilist2) -> ilist2
  |(ilist1, []) -> ilist1
  |(hd1::tl1, hd2::tl2) ->
    (if hd1 > hd2 
      then hd1::merge (tl1, hd2::tl2)
      else hd2::merge (hd1::tl1, tl2))

(* test : test function *)
(*
let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline ()))
    else ((print_string ("wrong answer")); (print_newline ()))

let _ =
  let test_merge = test merge in
  (test_merge ([9;7;5;3;1], [8;6;4;2;0]) [9;8;7;6;5;4;3;2;1;0]);
  (test_merge ([9;8;5;2;1;0], [7;6;4;3]) [9;8;7;6;5;4;3;2;1;0]);
  (test_merge ([], []) []);
  (test_merge ([], [8;6;4;2;0]) [8;6;4;2;0]);
  (test_merge ([9;7;5;3;1], []) [9;7;5;3;1]);
*)
