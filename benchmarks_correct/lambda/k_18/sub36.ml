type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let extend x l = [x]@l
let rec lookup x l =
  match l with
    |[] -> false
    |hd::tl -> if x = hd then true else lookup x tl

let rec check_help 
= fun lam lst ->
  match lam with
    |V x -> lookup x lst
    |P (x,la) -> check_help la (extend x lst)
    |C (la1,la2) -> check_help la1 lst && check_help la2 lst
  
    
let check : lambda -> bool
= fun lam -> check_help lam [];;
    
let p1 =  P ("a", V "a");;
let p2 =  P ("a", P ("a", V "a"));;
let p3 =  P ("a", P ("b", C (V "a", V "b")));;
let p4 =  P ("a", C (V "a", P ("b", V "a")));;

let p5 =  P ("a", V "b");;
let p6 =  P ("a", C (V "a", P ("b", V "c")));;
let p7 =  P ("a", P ("b", C (V "a", V "c")));;

check p1;;
check p2;;
check p3;;
check p4;;
check p5;;
check p6;;
check p7;;