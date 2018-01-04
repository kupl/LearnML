type nat = ZERO
         | SUCC of nat

let rec natadd (pair : nat * nat ) : nat =
  let (l,r) = pair in
  match l  with
  |ZERO -> r
  |SUCC(n) -> natadd(n, SUCC(r))

let rec natmul (pair: nat * nat ) : nat =
  let (l,r) = pair in
  match l  with
  |ZERO -> ZERO
  |SUCC(n) -> natadd(r, natmul(n,r))


(*
let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline ()))
    else ((print_string ("wrong answer")); (print_newline ()))

let _ =  
  let test_add = test natadd in
  (test_add ( (SUCC(SUCC(ZERO))), (SUCC(SUCC(SUCC(ZERO)))) ) 
  (SUCC(SUCC(SUCC(SUCC(SUCC(ZERO)))))) );
  (test_add ( (SUCC(ZERO)), (SUCC(SUCC(SUCC(ZERO)))) ) 
  (SUCC(SUCC(SUCC(SUCC(ZERO))))) )

let _ =  
  let test_mul = test natmul in
  (test_mul ( (SUCC(SUCC(ZERO))), (SUCC(SUCC(SUCC(ZERO)))) ) 
  (SUCC(SUCC(SUCC(SUCC(SUCC(SUCC(ZERO))))))) );
  (test_mul ( (SUCC(ZERO)), (SUCC(SUCC(SUCC(ZERO)))) ) 
  (SUCC(SUCC(SUCC(ZERO)))) )
*)
