
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

module VarMap = Map.Make(String)

let add_to_map a m =
VarMap.add a false m

let first_el k xo yo =
match xo,yo with
| Some x, Some y -> Some x
| None, yo -> yo
| xo, None -> xo

let get_1 (a,_) = a

let sat : formula -> bool = fun f ->
let rec build_map f m =
match f with
|Var(x) -> add_to_map x m
|And(x1, x2) -> VarMap.merge first_el (build_map x1 m) (build_map x2 m)
|Or(x1, x2) -> VarMap.merge first_el (build_map x1 m) (build_map x2 m)
|Imply(x1, x2) -> VarMap.merge first_el (build_map x1 m) (build_map x2 m)
|Iff(x1, x2) -> VarMap.merge first_el (build_map x1 m) (build_map x2 m)
|Neg(x) -> build_map x m
|True -> m
|False -> m in
let rec eval_formula f m =
match f with
|Var(x) -> VarMap.find x m
|And(x1, x2) -> (eval_formula x1 m) && (eval_formula x2 m)
|Or(x1, x2) -> (eval_formula x1 m) || (eval_formula x2 m)
|Imply(x1, x2) -> eval_formula (Or(Neg(x1), x2)) m
|Iff(x1, x2) -> eval_formula (Or(And(x1, x2),And(Neg(x1),Neg(x2)))) m
|Neg(x) -> not (eval_formula x m)
|True -> true
|False -> false in
let rec loop f l m =
if VarMap.is_empty l then eval_formula f m
else if loop f (VarMap.remove (get_1 (VarMap.choose l)) l) m then true
else if loop f (VarMap.remove (get_1 (VarMap.choose l)) l) (VarMap.add (get_1 (VarMap.choose l)) true m) then true
else false in
let map = VarMap.empty in
let map = build_map f map in
loop f map map

