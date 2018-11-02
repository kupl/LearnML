(* problem 3*)
type formula =
True
| False
| Var of string
| Neg of formula
| And of formula * formula
| Or of formula * formula
| Imply of formula * formula
| Iff of formula * formula

let sat : formula -> bool 
= fun f ->
let rec varfinder1 : formula -> (formula * bool)list -> (formula * bool)list -> (formula * bool)list 
= fun var l1 l2 -> 
  match l1 with
  | [] -> [(var, true)]@l2
  | (a, b)::tail -> 
    if (a = var) then l2
    else varfinder1 var tail l2 in

    let rec varfinder2 : formula -> (formula * bool) list -> bool 
    = fun var l -> 
      match l with
      | [] -> true
      | (a, b)::tail -> 
        if var = a then b
        else varfinder2 var tail in


      let rec appendlist : formula -> (formula * bool) list -> (formula * bool) list 
      = fun f l -> 
        match f with
        | True -> []
        | False -> []
        | Var a -> varfinder1 (Var a) [] []
        | Neg a -> appendlist a l
        | And (a, b) -> (appendlist a l)@(appendlist b l)
        | Or (a, b) -> (appendlist a l)@(appendlist b l)
        | Imply (a, b) -> (appendlist a l)@(appendlist b l)
        | Iff (a, b) -> (appendlist a l)@(appendlist b l) in

let setter = appendlist f [] in

let tfswap : (formula * bool) list -> (formula * bool) list = fun l
-> match l with
| [] -> []
| (a, b)::tail -> if b = true then ((a, false)::tail) else tail in

let rec showstate : formula -> (formula * bool) list -> bool = fun f table
-> match f with
| True -> true
| False -> false
| Var a -> varfinder2 (Var a) table
| Neg a -> not(showstate a table)
| And (a, b) -> (showstate a table) && (showstate b table)
| Or (a, b) -> (showstate a table) || (showstate b table)
| Imply (a, b) -> if ((showstate a table) = true) && ((showstate b table) = false) then false else true
| Iff (a, b) -> if (showstate a table) = (showstate b table) then true else false in

let rec test : (formula * bool) list -> bool = fun l
-> match l with
| [] -> false
| (a,b)::tail -> b || (test tail) in

let rec result : formula -> (formula * bool) list -> bool = fun f l
-> if (test l) = false then showstate f l
else (showstate f l) || (result f (tfswap l)) in
result f setter
