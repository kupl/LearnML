type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string


let rec check_list((l : var list), id) : bool =
  match l with
  |[] -> false
  |hd::tl -> if (hd <> id) then check_list(tl, id)
             else  true;;
 let rec check_list((l : var list), (m : lambda)) : bool = 
  match m with
  | V(id) -> if check_list(l, id) then true
                   else false
  | P(id, m) -> check_list(id::l , m)
  | C(m1, m2) -> check_list(l, m1) && check_list(l, m2);;

  let rec check(m: lambda) : bool =
    match m with
    | V(id) -> false
    | P(id, m) -> check_list(id::[], m)
    | C(m1, m2) -> check(m1) && check(m2);;
    