type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string


let rec checking lam lst = match lst with
  | [] -> (match lam with
    | V x -> false
    | P (x, l) -> checking l (x::lst)
    | C (l1, l2) -> if (checking l1 lst = true)&&(checking l2 lst = true) then true else false)
  | hd::tl -> match lam with
    | V x -> if x = hd then true else checking lam tl
    | P (x, l) -> checking l (x::lst)
    | C (l1, l2) -> if (checking l1 lst = true)&&(checking l2 lst = true) then true else false;;
    
    
let rec check : lambda -> bool
= fun lam -> checking lam [];;
  

check (P("a", V "a"));;
check (P("a", P("a", V "a")));;
check (P("a", P("b", C(V "a", V"b"))));;
check (P("a", C(V "a", P("b", V "a"))));;

check (P("a", V"b"));;
check (P("a", C(V "a", P("b", V "c"))));;
check (P("a", P("b", C(V"a", V"c"))));;
    

