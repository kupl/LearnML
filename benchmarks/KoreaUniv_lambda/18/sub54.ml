type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec f : (var * bool)list -> lambda -> bool
= fun lst lam -> match lam with
  | V(var) -> (try
                List.assoc var lst
              with Not_found -> false)
  | P(v,l) -> f ((v,true)::lst) l 
  | C(l1,l2) -> let a = f lst l1 in
                let b = f lst l2 in
                (if a=true && b=true then true
                 else false);;
                      

let check : lambda -> bool
= fun lam -> match lam with
  | V(var) -> false
  | P(v,l) -> f [(v,true)] l
  | C(l1,l2) -> f [] lam;;
  
let a =  C(P("a",V "a"),P("a",V "a")) in
check a;;

