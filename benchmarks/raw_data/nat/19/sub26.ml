type nat = ZERO | SUCC of nat

let rec count l =
 match l with
  | ZERO -> 0
  | SUCC a -> 1 + count a
;;

let rec write k = 
 match k with
  | 0 -> ZERO
  | _ -> SUCC (write (k-1))
;; 

let rec natadd : nat -> nat -> nat
  = fun n1 n2 ->
    write (count n1 + count n2)
;;

let natmul : nat -> nat -> nat
  = fun n1 n2 -> 
    write (count n1 * count n2)
;;
    
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natmul two three;;
natadd two three;;


