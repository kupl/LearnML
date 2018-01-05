let rec merge (pair : int list * int list) : int list =
  let (a,b) = pair in
  match (a,b) with
  |([],[]) -> []
  |([],hd::tl) -> hd::tl
  |(hd::tl,[]) -> hd::tl
  |(hd1::tl1,hd2::tl2) ->
    if (hd1 > hd2) then
      hd1 :: merge(tl1, hd2::tl2)
    else
      hd2 :: merge(tl2, hd1::tl1)

(*
let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline ()))
    else ((print_string ("wrong answer")); (print_newline ()))

let _ =
  let test_merge = test merge in
  (test_merge ([5;3;1],[4;2]) [5;4;3;2;1]);
  (test_merge ([3;2;1],[]) [3;2;1]);
  (test_merge ([],[10;5;3]) [10;5;3]);
  (test_merge ([],[]) []);
  (test_merge ([10;4;2;1],[8;7;2]) [10;8;7;4;2;2;1]);
  (test_merge ([11;8;3],[9;5;3;1]) [11;9;8;5;3;3;1])
*)
