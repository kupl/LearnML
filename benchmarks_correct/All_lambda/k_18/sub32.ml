type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam ->
  let rec allbound : var list -> lambda -> bool
  = fun varlst expr ->
    match expr with
      | V v -> if ((List.filter (fun x -> if (x=v) then true else false) varlst)=[]) then false else true
      | P (x,nexpr) -> allbound (x::varlst) nexpr
      | C (expr1,expr2) -> (allbound varlst expr1) && (allbound varlst expr2)
  in allbound [] lam;;
