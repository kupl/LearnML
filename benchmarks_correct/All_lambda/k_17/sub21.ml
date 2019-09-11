(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> true (* TODO *)
     let rec check_2 (lam,l) =
    match lam with
        V v ->
        (match l with
        [] -> false
        |h::t -> if v=h then true else check_2 (V v,t))
        | P (v,lam) -> check_2 (lam, v::l)
        | C (lam1,lam2) -> if check_2 (lam1,l)=true && check_2 (lam2,l)=true then true
else false;;

    let check : lambda -> bool
    = fun lam -> check_2 (lam,[]);; 