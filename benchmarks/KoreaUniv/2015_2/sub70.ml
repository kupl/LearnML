let rec filter pred lst
=match lst with
	 		|[]->[]
	 		|hd::tl->
				if ((pred hd)=true) then (hd)::(filter pred tl)
				else filter pred tl
;;

(*Problem 2 :zipper*) 
let rec zipper : int list * int list -> int list
=fun (a,b) ->
			match (a,b) with
			|([],[])->[]
			|([],b)->b
			|(a,[])->a
			|(hd1::tl1),(hd2::tl2)->
					 (hd1)::(hd2)::(zipper (tl1,tl2))
;;


(*Problem 3:iter*)
let rec iter : int * (int ->int) -> (int->int)
=fun (n,f)->
			match n with
			|0-> fun x->x
			|1-> f
			|_-> let k  = iter (n-1,f) in (fun x-> k (f x))
;;

(*Problem 4:Diff*)
type aexp=
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

exception InvalidArgument

let rec diff : aexp * string ->aexp
=fun (aexp,x)->
					match aexp with
					|Const k -> Const 0
					|Var str -> if str=x then Const 1 else Const 0
					|Power (str, n)->
							if str=x then
										if n=0 then Const 0
										else if n=1 then Const 0
										else Times [Const n;Power (str,n-1)]
							else Const 0
					|Times lst->
							(if (List.mem (Const 0) lst) then Const 0
							else
							match lst with
							|[]->raise InvalidArgument
							|hd:: [] -> diff (hd, x)
							|hd ::tl->
									match hd with
									|Const 1 -> diff ((Times tl),x)
									|Const k -> Times [hd;diff (Times tl, x)]
									|_-> Sum([(Times ((diff (hd, x))::tl); (Times [hd; (diff ((Times tl),x))]))])
					)
					|Sum lst ->
							match lst with
							|[]-> raise InvalidArgument
							|hd::[] -> diff (hd, x)
							|hd::tl -> Sum [(diff (hd, x)) ; (diff((Sum tl), x)) ]
;;



(*Problem 5:Calculator*)
type exp=X
		|INT of int
		|ADD of exp * exp
		|SUB of exp * exp
		|MUL of exp * exp
		|DIV of exp * exp
		|SIGMA of exp * exp * exp

let rec mecalculate
= fun (exp, x)->
			match (exp, x) with
			|(X, INT x)-> x
			|(INT k, _) -> k
			|(ADD (e1, e2),_)->
						(match e1, e2 with
						|INT k, INT n -> k+n
						|_,_ ->mecalculate (e1, x)+ mecalculate (e2, x))
			|(SUB (e1, e2),_)->
						(match e1, e2 with
						|INT k, INT n -> k-n
						|_, _ -> mecalculate (e1, x)- mecalculate (e2, x))
			|(MUL (e1, e2),_) ->
						(match e1, e2 with
						|INT x, INT y -> x*y
						|_,_ -> mecalculate (e1, x) * mecalculate (e2, x))
			|(DIV (e1, e2),_) ->
						(match e1, e2 with
						|INT x, INT y -> x/y
						|_,_ -> mecalculate (e1, x) / mecalculate (e2, x))
			|(SIGMA (exp1, exp2, exp3),_)->
						let  a= mecalculate (e1, x) and b=mecalculate (e2, x) in
						( if a<b then 0
							else if (a=b) then mecalculate (exp3, INT a)
							else mecalculate (exp3, INT a) + mecalculate (SIGMA (INT (a+1), INT b, exp3), x))

let calculator :exp -> int
=fun f-> mecalculate (f, X)

;;

