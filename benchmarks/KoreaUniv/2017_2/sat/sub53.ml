(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula;;

let rec proposition : formula -> string list
= fun f ->
match f with
True | False -> []
| Neg a -> proposition a
| And (a,b) | Or (a,b) | Imply (a,b) | Iff (a,b) -> proposition a @ proposition b
| Var x -> [x];;

 let rec unique x =
let rec uniq l n =
match l with
[] -> []
| h::t -> if n = h then uniq t n
else h::(uniq t n)
in match x with
[] -> []
|h::t -> h::(uniq (unique t) h);;

let prop f = unique (proposition f);;

let rec apply x e =
match e with
[] -> raise (Failure("ERROR"))
| (y,v)::tl -> if x = y then v else apply x tl;;

 let rec eval f e =
match f with
True -> true
| False -> false
| And(a,b) -> if eval a e && eval b e then true else false
| Or(a,b) -> if not (eval a e) && not (eval b e) then false else true
| Neg a -> if eval a e then false else true
| Imply(a,b) -> if eval a e && eval b e then true
else if not (eval a e) then true
else false
| Iff(a,b) -> if eval a e && eval b e then true
else if not (eval a e) && not (eval b e) then true
else false
| Var x -> eval (apply x e) e;;

let rec combinations = function
 | 0 -> [[]]
 | n ->
let rest = combinations (n-1) in
let comb_f = List.map (fun l -> False::l) rest in
let comb_t = List.map (fun l -> True::l) rest in
comb_t @ comb_f;;

let rec zip (a,b) =
match (a,b) with
([],[]) -> []
| ([], a::b) -> []
| (a::b, []) -> []
| (a::b, c::d) -> (a,c)::zip(b,d);;

let rec check f x =
match x with
[[]] -> true
| [] -> false
| hd::tl -> if eval f (zip(prop f,hd)) then true else check f tl;;


let sat : formula -> bool
= fun f -> check f (combinations (List.length (prop f)));;

