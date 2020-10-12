type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec findlst : var list -> var -> bool
= fun lst v ->
    match lst with
      | [] -> false
      | hd::tl -> if (hd = v) then true else (findlst tl v)

let rec mycheck : lambda -> var list -> bool
= fun lam lst -> 
    match lam with
      | V x -> (findlst lst x)
      | P (x, e) -> (mycheck e (x::lst))
      | C (e1, e2) -> (mycheck e1 lst) && (mycheck e2 lst)

let check : lambda -> bool
= fun lam -> (mycheck lam []);;

let t1 = P ("a", V "a");;
let t2 = P ("a", P ("a", V "a"));;
let t3 = P ("a", P ("b", C (V "a", V "b")));;
let t4 = P ("a", C (V "a", P ("b", V "a")));;

let t5 = P ("a", V "b");;
let t6 = P ("a", C (V "a", P ("b", V "c")));;
let t7 = P ("a", P ("b", C (V "a", V "c")));;

check t1;;
check t2;;
check t3;;
check t4;;

check t5;;
check t6;;
check t7;;
