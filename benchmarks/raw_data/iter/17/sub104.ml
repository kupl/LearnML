(* 2014-17189 이소희
 * Exercise 1-3, Due: 9/14, 24:00 *)

let compose f g = (fun x -> f (g x))

let rec iter ((n : int), (f : 'a -> 'a)) : 'a -> 'a =
  if (n <= 0) 
    then (fun x -> x)
    else compose f (iter (n-1, f))

(* test : test function *)
(*
let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline ()))
    else ((print_string ("wrong answer")); (print_newline ()))

let inc3 = fun x -> x + 3
let double = fun x -> 2 * x

let _ =
  (test (iter (0, inc3)) 104 104);
  (test (iter (-1, double)) 203 203);
  (test (iter (11, inc3)) 0 33);
  (test (iter (4, double)) 100 1600);
*)
