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
let l = 
let rec findVars f l = match f
with Var x -> if ( let rec findVar k = match k
with hd::tl -> if (hd = x) then true else findVar tl
| [] -> false
in findVar l ) then l 
else x::l
| Neg a -> findVars a l
| And (a,b) ->  findVars b (findVars a l)
| Or (a,b) ->  findVars b (findVars a l)
| Imply (a,b) ->  findVars b (findVars a l)
| Iff (a,b) -> findVars b (findVars a l)
| _ -> []
in findVars f []
in let rec logic f cl = match f
with Var x -> (let rec findVal cl = match cl
with hd::tl -> (match hd 
with (a,b) -> if (a = x) then b else findVal tl)
| [] -> false
in findVal cl)
| Neg x -> not (logic x cl)
| And (a, b) -> (logic a cl) && (logic b cl)
| Or (a, b) -> (logic a cl) || (logic b cl)
| Imply (a, b) -> (not (logic a cl)) || (logic b cl)
| Iff (a, b) -> let iff p q = (not p || q) && (not q || p) in iff (logic a cl)  (logic b cl)
| False -> false
| True -> true
in let rec table l cl = match l
with hd::tl -> (table tl ((hd, true)::cl)) || (table tl ((hd, false)::cl))
| [] -> logic f cl
in table l []
