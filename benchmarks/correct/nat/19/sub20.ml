type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
   match n1 with
     | ZERO -> n2
     | SUCC n -> let new_n2 = SUCC(n2) in natadd n new_n2;; 

(*netadd 이용*)
let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  
  let rec calcu cnt_mul base result=
    match cnt_mul with
      | ZERO -> result
      | SUCC n -> 
        let new_result = (natadd base result) 
        in calcu n base new_result
  in calcu n1 n2 ZERO;;

(*output*)
let two = SUCC (SUCC ZERO);;
let three = SUCC(two);;

let add = natadd two three;;

let mul = natmul three three;;