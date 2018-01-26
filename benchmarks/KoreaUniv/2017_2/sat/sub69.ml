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

let sat : formula -> bool = fun f
->
let rec findvar : formula -> (formula * bool)list -> (formula * bool)list -> (formula * bool)list = fun var l1 l2
->match l1 with
| [] -> [(var, true)]@l2
| (a, b)::tail -> if (a = var) then l2
        else findvar var tail l2 in

let rec findvar2 : formula -> (formula * bool) list -> bool = fun var l
-> match l with
| [] -> true
| (a, b)::tail -> if var = a then b
        else findvar2 var tail in


let rec mlist : formula -> (formula * bool) list -> (formula * bool) list = fun f l
-> match f with
| True -> []
| False -> []
| Var a -> findvar (Var a) [] []
| Neg a -> mlist a l
| And (a, b) -> (mlist a l)@(mlist b l)
| Or (a, b) -> (mlist a l)@(mlist b l)
| Imply (a, b) -> (mlist a l)@(mlist b l)
| Iff (a, b) -> (mlist a l)@(mlist b l) in

let onepos = mlist f [] in

let tablechange : (formula * bool) list -> (formula * bool) list = fun l
-> match l with
| [] -> []
| (a, b)::tail -> if b = true then ((a, false)::tail) else tail in

let rec exetable : formula -> (formula * bool) list -> bool = fun f table
-> match f with
| True -> true
| False -> false
| Var a -> findvar2 (Var a) table
| Neg a -> not(exetable a table)
| And (a, b) -> (exetable a table) && (exetable b table)
| Or (a, b) -> (exetable a table) || (exetable b table)
| Imply (a, b) -> if ((exetable a table) = true) && ((exetable b table) = false) then false else true
| Iff (a, b) -> if (exetable a table) = (exetable b table) then true else false in

let rec test : (formula * bool) list -> bool = fun l
-> match l with
| [] -> false
| (a,b)::tail -> b || (test tail) in

let rec exeall : formula -> (formula * bool) list -> bool = fun f l
-> if (test l) = false then exetable f l
else (exetable f l) || (exeall f (tablechange l)) in
exeall f onepos