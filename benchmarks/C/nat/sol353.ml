type nat = ZERO | SUCC of nat;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
let four = SUCC (SUCC (SUCC (SUCC ZERO)));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC s1 -> SUCC (natadd s1 n2);;
    
natadd two three;;(*TODO*)

let rec nadd nt1 nt2 n =
  if n = 1 then nt1
  else nadd (natadd nt1 nt2) nt2 (n-1)
  
let rec length n =
  match n with
    | ZERO -> 0
    | SUCC s1 -> 1 + length s1;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
   match length n2 with
     | 0 -> ZERO
     | _ -> nadd n1 n1 (length n2);;(*TODO*)

natmul two three;;
natmul three three;;
natmul four two;;
natmul three four;;