
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  

  let check : exp -> bool
  =fun e -> true;;


  let rec isFreeVar : var * string list -> bool 
  =fun (v, l) -> match l with
                            | [] -> false
                            | hd :: tl -> if (v = hd) then true else isFreeVar (v,tl);;

  let rec findVar : exp * string list -> bool
  = fun (e, l) -> match e with
                  | V (a) -> isFreeVar (a, l)
                  | P (v,e1) -> findVar (e1, (l @ [v]))
                  | C (e1,e2) -> if (findVar(e1,l) && findVar(e2,l))= true then true else false;;


  let check  : exp -> bool
  =fun e -> findVar (e, []);;
