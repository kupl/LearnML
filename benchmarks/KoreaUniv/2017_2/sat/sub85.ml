(*problem 3*)

type formula = True | False | Var of string | Neg of formula 
				| And of formula * formula | Or of formula * formula
				| Imply of formula * formula | Iff of formula * formula
type env = (string * formula) list

let update_env : (string * formula)->(string * formula) list->(string * formula) list = fun (a,b) env->
	(a,b)::env;;

let rec find_env : string->(string * formula) list->formula = fun s env->
	match env with 
	|[]->raise(Failure "error")
	|(x,en)::tl->(if s=x then en
				else find_env s tl
			)
;;

let rec overlap : string list -> string -> formula = fun lst s ->
match lst with 
	|[]->False
	|hd::tl->if hd = s then True
			else overlap tl s
;;
let rec varlst : formula -> string list ->string list= fun f lst->
match f with
	|True -> lst
	|False -> lst
	|Var x-> if (overlap lst x) = False then x::lst
			else lst
	|Neg a->varlst a lst
	|And (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
				else varlst a lst@varlst b lst)
	|Or (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
				else varlst a lst@varlst b lst)
	|Imply (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
				else varlst a lst@varlst b lst)
	|Iff (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
				else varlst a lst@varlst b lst)
;;

let rec length : 'a list -> int = fun lst->
match lst with |[]->0 |hd::tl->1+(length tl);;

let rec length_plus : string list->formula list->formula list = fun slst flst->
if length slst != length flst then ([False]@flst)
else flst
;;

let rec dec_to_formula : int->formula list = fun n->
if (n/2 != 1 && n/2 !=0 && n mod 2 = 0) then dec_to_formula(n/2)@[False]
else if (n/2 != 1 && n/2 !=0 && n mod 2 = 1) then dec_to_formula(n/2)@[True]
else if (n/2 = 1 && n mod 2 = 0) then [True;False]
else if (n/2 = 1 && n mod 2 = 1) then [True;True]
else if n = 1 then [True]
else [False]
;;

let rec mkenv : string list -> formula list ->(string*formula) list  = fun lst flst->
	match lst with 
	|hd::tl->(match flst with
				|hd2::tl2->(hd,hd2)::mkenv tl tl2
				|[]->(hd,False)::mkenv tl []
			)
	|[]->[]
;;
let rec sat2 : formula -> env-> formula = fun f env->
match f with
	|True -> True
	|False -> False
	|Var x-> find_env x env 
	|Neg a -> if (sat2 a env)=True then False
				else True
	|And (a,b) -> let (c,d) = (sat2 a env,sat2 b env) in
					(match (c,d) with 
						|(True,True)->True
						|(True,False)->False
						|(False,True)->False
						|(False,False)->False
					)
	|Or (a,b) -> let (c,d) = (sat2 a env,sat2 b env) in
					(match (c,d) with 
						|(True,True)->True
						|(True,False)->True
						|(False,True)->True
						|(False,False)->False
					)
	|Imply (a,b)-> let (c,d) = (sat2 a env,sat2 b env) in
					(match (c,d) with 
						|(True,True)->True
						|(True,False)->False
						|(False,True)->True
						|(False,False)->True
					)
	|Iff (a,b)->let (c,d) = (sat2 a env,sat2 b env) in
					(match (c,d) with 
						|(True,True)->True
						|(True,False)->False
						|(False,True)->False
						|(False,False)->True
					)
;;
let rec sat3 : string list->formula ->int ->formula list->formula list = fun slst f n flst->
let a = length slst in
	let b = (expo 2 a) in
		(if(n!=b) then
			[(sat2 f (mkenv slst (length_plus slst (dec_to_formula n))))]@
			(sat3 slst f (n+1) flst)
		else 
		[]
		)
;;
let rec find_true : formula list->bool = fun flst->
match flst with 
	|[]->false
	|hd::tl->(if hd=True then true
			else find_true tl
	)
;;
let rec sat : formula -> bool = fun f->
	let a = (varlst f []) in
		let b= sat3 a f 0 [] in
		find_true b
;;