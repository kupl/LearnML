(* problem 2*)
type nat = ZERO | SUCC of nat

let rec find_num : nat -> int
= fun n1 -> match n1 with
            |ZERO -> 0
            |SUCC(n2) -> 1 + find_num n2

let rec addnat : int -> nat
= fun n1 -> if n1=0 then ZERO else SUCC(addnat (n1-1))

let rec exp : int -> int -> int
= fun n1 n2 -> if n2=0 then 1 else n1 * (exp n1 (n2-1))
  
let natadd : nat -> nat -> nat 
= fun n1 n2 -> addnat ((find_num n1) + (find_num n2))

let natmul : nat -> nat -> nat 
= fun n1 n2 -> addnat ((find_num n1) * (find_num n2))

let natexp : nat -> nat -> nat 
= fun n1 n2 -> addnat (exp (find_num n1) (find_num n2))