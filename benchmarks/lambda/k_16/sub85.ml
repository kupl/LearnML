
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec evaluate_vpc lambdaression vpc =
    match lambdaression with
    |V a -> let rec check lst st = 
          match lst with      
          [] -> false 
          | head::tail -> if head=st then true else (check tail st) in if (check vpc a) then true else false 
    | P (pa, ex1) -> if ( evaluate_vpc ex1 (vpc @ [pa]) ) then true else false
    | C (ex1, ex2) -> if (evaluate_vpc ex1 vpc && evaluate_vpc ex2 vpc) then true else false;;

  let check : lambda -> bool
  = fun lambda -> evaluate_vpc lambda [];; (* raise NotImplemented *) (* TODO *)

