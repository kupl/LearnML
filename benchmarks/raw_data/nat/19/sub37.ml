type nat = ZERO | SUCC of nat


let rec nlength n =
  match n with
    ZERO -> 0
  | SUCC n -> 1 + nlength n;;
  
let rec maken : int -> nat
= fun i ->
    match i with
      0 -> ZERO
    | _ -> SUCC (maken (i-1));;
    
    

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  maken (nlength n1 + nlength n2);;

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  maken (nlength n1 * nlength n2);;



    
let two = SUCC(SUCC ZERO) and three = SUCC(SUCC(SUCC ZERO));;

natadd two three;;
natadd (SUCC(SUCC ZERO)) (SUCC(SUCC(SUCC ZERO)));;

natmul two three;;
natmul (SUCC(SUCC ZERO)) (SUCC(SUCC(SUCC ZERO)));;


