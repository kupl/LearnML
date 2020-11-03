type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec lookup ls v =
  match ls with
    |[] -> false
    |hd::tl ->  if hd = v then true else lookup tl v
    
let extend ls v = ls @ [v];;

let check : lambda -> bool
= fun lam ->
  let rec ch vlist lamb =
    match lamb with
      | V x -> lookup vlist x
      | P (nv,nb) -> ch (extend vlist nv) nb
      | C (a, b) -> ch vlist a && ch vlist b
      | _ ->raise ( Failure("Undefined semantics"))
  in ch [] lam ;;
  






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
