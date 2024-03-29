
  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string

  let check : lambda -> bool
  =fun e -> true;;

  let rec isFree : var * string list -> bool 
  =fun (v, l) -> match l with
    | [] -> false
    | hd :: tl -> if (v = hd) then true else isFree (v,tl);;

  let rec checkForm : lambda * string list -> bool
  = fun (e, l) -> match e with
    | V (a) -> isFree (a, l)
    | P (v, e1) -> checkForm (e1, (l @ [v]))
    | C (e1, e2) -> if ((checkForm(e1, l) && checkForm(e2, l)) = true) then true else false;;

  let check  : lambda -> bool
  =fun e -> checkForm (e, []);;
