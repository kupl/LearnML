type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 ->let rec js=fun x1 x2->if x1<>n1 then js (SUCC x1) (SUCC x2)else x2 in js ZERO n2;; 
(*TODO*)

let natmul : nat -> nat -> nat
= fun n1 n2 ->let rec js=fun x1 x2->if x2<>n2 then js(natadd x1 n1) (SUCC x2) else x1 in js ZERO ZERO;;
(*TODO*)


let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natadd two three;;
natmul two three;;