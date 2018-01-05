(* 2014-17189 이소희
 * Exercise 1-4, Due: 9/14, 24:00 *)

type nat =
  | ZERO
  | SUCC of nat

let rec natadd ((m : nat), (n : nat)) : nat =
  match m with
  | ZERO -> n
  | SUCC mp -> SUCC (natadd (mp, n))

let rec natmul ((m : nat), (n : nat)) : nat =
  match m with
  | ZERO -> ZERO
  | SUCC mp -> natadd (n, (natmul (mp, n)))

(* test : test function *)
(*
let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline ()))
    else ((print_string ("wrong answer")); (print_newline ()))

let _ =
  let test_natadd = test natadd in
  (test_natadd (ZERO, ZERO) ZERO);
  (test_natadd (SUCC (SUCC (SUCC ZERO)), ZERO) (SUCC (SUCC (SUCC ZERO))));
  (test_natadd (ZERO, SUCC (SUCC (SUCC (SUCC ZERO)))) (SUCC (SUCC (SUCC (SUCC ZERO)))));
  (test_natadd ((SUCC (SUCC ZERO)), (SUCC ZERO)) (SUCC (SUCC (SUCC ZERO))));
  
  let test_natmul = test natmul in
  (test_natmul (ZERO, ZERO) ZERO);
  (test_natmul (SUCC (SUCC (SUCC ZERO)), ZERO) ZERO);
  (test_natmul (ZERO, SUCC (SUCC (SUCC (SUCC ZERO)))) ZERO);
  (test_natmul ((SUCC (SUCC ZERO)), (SUCC ZERO)) (SUCC (SUCC ZERO)));
  (test_natmul ((SUCC (SUCC (SUCC ZERO))), (SUCC (SUCC ZERO))) (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))))); 
*)
