
(* problem 3*)
 
type formula =
True | False | Var of string | Neg of formula | And of formula * formula
| Or of formula * formula | Imply of formula * formula
| Iff of formula * formula

let rec sat : formula -> bool = fun t ->
let rec tab : formula -> (string * bool) list = fun r ->
let table = [] in
match r with
|Var x -> (x, true) :: table
|And (x,y) -> (tab x) @ (tab y)
|Or (x,y) -> (tab x) @ (tab y)
|Imply (x,y) -> (tab x) @ (tab y)
|Iff (x,y) -> (tab x) @ (tab y)
|Neg x -> tab x
|_ -> table
in
let rec apply x e =
match e with
|[] -> raise (Failure("error"))
|(y,v)::tl -> if (x=y) then v else apply x tl
in
let rec check : formula -> (string * bool) list -> bool = fun r t ->
match r with
| True -> true
| False -> false
| And(x,y) -> (check x t) && (check y t)
| Or(x,y) -> (check x t) || (check y t)
| Imply (x,y) -> (not (check x t)) || (check y t)
| Iff (x,y) -> ((check x t) || (check y t))&&((not (check x t)) ||
(not (check y t)))
| Neg x -> not (check x t)
| Var x -> apply x t
in
let rec change : (string * bool) list -> int -> bool ->
(string * bool) list -> (string * bool) list  = fun t i r s->
if (i = (List.length t) - 1) then match (List.nth t i) with
|(x,true) -> change t (i-1) true ((x,false)::s)
|(x,false) -> change t (i-1) false ((x,true)::s)
else if (i>=0) then match (List.nth t i) with
|(x,true) -> change t (i-1) true ((x,r)::s)
|(x,false) -> change t (i-1) r ((x,(not r))::s)
else s
in
let rec program : formula -> (string * bool) list -> int -> bool
= fun f t i ->
if(i<0) then false
else if(check f t = true) then true
else program f (change t ((List.length t)-1) true []) (i-1)
in if(List.length (tab t) = 0) then check t [] else program t (tab t) (List.length (tab t)-1);;