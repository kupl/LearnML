(* 2014-17189 이소희
 * Exercise 1-2, Due: 9/14, 24:00 *)

let rec sigma f a b =
  if (a <= b)
    then (f b) + (sigma f a (b-1))
    else 0

(* test : test function *)
(*
let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline ()))
    else ((print_string ("wrong answer")); (print_newline ()))

let incr = fun x -> x + 1
let double = fun x -> 2 * x

let _ =
  let test_sigma = test sigma in
  (test_sigma (0, 9, incr) 55);
  (test_sigma (1, 10, double) 110);
  (test_sigma (3, 3, incr) 4);
  (test_sigma (3, -1, incr) 0);
*)
