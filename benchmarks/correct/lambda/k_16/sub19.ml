
  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  

  let check : lambda -> bool
  =fun e -> true;;


  let rec isFreeVar : var * string list -> bool 
  =fun (v, l) -> match l with
                            | [] -> false
                            | hd :: tl -> if (v = hd) then true else isFreeVar (v,tl);;

  let rec findVar : lambda * string list -> bool
  = fun (e, l) -> match e with
                  | V (a) -> isFreeVar (a, l)
                  | P (v,e1) -> findVar (e1, (l @ [v]))
                  | C (e1,e2) -> if (findVar(e1,l) && findVar(e2,l))= true then true else false;;


  let check  : lambda -> bool
  =fun e -> findVar (e, []);;
