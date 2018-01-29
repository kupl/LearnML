
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec evaluate_vpc expression vpc =
    match expression with
    |V a -> let rec check list st = 
          match list with      
          [] -> false 
          | head::tail -> if head=st then true else (check tail st) in if (check vpc a) then true else false 
    | P (pa, ex1) -> if ( evaluate_vpc ex1 (vpc @ [pa]) ) then true else false
    | C (ex1, ex2) -> if (evaluate_vpc ex1 vpc && evaluate_vpc ex2 vpc) then true else false;;

  let check : exp -> bool
  = fun exp -> evaluate_vpc exp [];; (* raise NotImplemented *) (* TODO *)

