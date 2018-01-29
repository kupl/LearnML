(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec reverse_list
= fun l ->
	match l with
	|[] -> []
	|hd::tl -> (reverse_list tl)@[hd]

let last2head
= fun l -> 
	match (reverse_list l) with
	|[] -> []
	|hd::tl -> hd::(reverse_list tl)

let rec map f l x=
	match l with
	|[] -> []
	|hd::tl -> (f (hd,x))::(map f tl x)

let rec diff_op : aexp * string -> aexp
= fun (e,x) ->
	match e with
	| Const n -> Const 0
	| Var v -> if x = v then Const 1 else Const 0
	| Power (v,n) ->
		if x = v then 
			(match n with
			|0 -> Const 0
			|1 -> Const 1
			|_ -> Times [Const n;Power (v,n-1)])
		else Const 0
	| Times l ->
		(match l with
		|[] -> Const 0
		|hd::tl -> let a = diff_op (hd,x) in
				   let b = diff_op ((Times tl),x) in
				   let l1 = Times (a::tl) in
				   let l2 = Times (hd::b::[]) in
				   Sum (l1::l2::[]))
	| Sum l -> Sum (map diff_op l x)	    

let get_head
= fun l -> 
	match l with
	|[] -> raise (Failure "no head")
	|hd::tl -> hd

let get_tail
= fun l ->
	match l with
	|[] -> []
	|hd::tl -> tl

let rec check_list
= fun l exp ->
	match l with
	|[] -> false
	|hd::tl -> if hd = exp then true
			   else check_list tl exp

let rec clean_timelist
= fun l ->
	match l with
	|[] -> []
	|hd::tl -> match hd with
			   | Const 1 -> tl
			   | Times nl -> clean_timelist nl@tl
			   | _ -> hd::(clean_timelist tl)

let rec clean_sumlist
= fun l ->
	match l with
	|[] -> []
	|hd::tl -> match hd with
			   | Const 0 -> tl
			   | Sum nl -> clean_sumlist nl@tl
			   | _ -> hd::(clean_sumlist tl)

let rec combine_timelist
= fun l n ->
	match l with
	|[] -> (Const n)::[]
	|hd::tl -> match hd with
			   |Const i -> combine_timelist tl (n*i)
			   |_ -> hd::(combine_timelist tl n)

let rec combine_sumlist
= fun l n ->
	match l with
	|[] -> (Const n)::[]
	|hd::tl -> match hd with
			   |Const i -> combine_sumlist tl (n+i)
			   |_ -> hd::(combine_sumlist tl n)

let rec combine
= fun (e,x) ->
	match e with
	| Times l ->
		let combinelist = last2head (combine_timelist l 1) in
			if (get_head combinelist) = Const 1 then Times (map combine (get_tail combinelist) x)
			else Times ((get_head combinelist)::(map combine (get_tail combinelist) x))
	| Sum l ->
		let combinelist = clean_sumlist (combine_sumlist l 0) in
			Sum (map combine combinelist x)
	| _ -> e

let rec cleanup
= fun (e,x) ->
	match e with
	| Const n -> Const n
	| Var x -> Var x
	| Power (x,n) ->
		(match n with
		| 0 -> Const 1
		| 1 -> Var x
		| _ -> Power (x,n))
	| Times l ->
		let cleanlist = clean_timelist l in
			if cleanlist = [] then Const 1
			else if (get_tail cleanlist) = [] then cleanup ((get_head cleanlist),x) 
			else if check_list cleanlist (Const 0) then Const 0
				else Times (map cleanup cleanlist x)
	| Sum l ->
		let cleanlist = clean_sumlist l in
			if cleanlist = [] then Const 0
			else if (get_tail cleanlist) = [] then cleanup ((get_head cleanlist),x) 
			else Sum (map cleanup cleanlist x)

let rec ultraclean
= fun (e,x) ->
	let ce = cleanup (e,x) in
		if ce = e then ce
		else ultraclean (ce,x)

let rec diff : aexp * string -> aexp
= fun (e,x) -> combine(ultraclean ((diff_op (e,x)),x), x)