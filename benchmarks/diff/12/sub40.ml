type aexp =
	| Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec diff (aexp, str) =
	match aexp with
	| Const i -> Const 0
	| Var var ->
		if var = str
			then Const 1
			else Const 0
	| Power (var, pow) ->
		if var = str
			then
				if pow = 1
					then Const 1
					else Times [Const pow; Power (var, pow-1)]
			else Const 0
	| Times lst ->
		let rec diff_product left_elts right_elts =
			if right_elts = []
				then []
				else
					let hd = List.hd right_elts in
					let tl = List.tl right_elts in
					(Times (List.rev_append left_elts (diff (hd, str)::tl)))::( diff_product (hd::left_elts) tl )
		in
		Sum (diff_product [] lst)
	| Sum lst ->
		let rec diff_sum elts =
			if elts = []
				then []
				else
					let hd = List.hd elts in
					let tl = List.tl elts in
					(diff (hd, str))::(diff_sum tl)
		in
		Sum (diff_sum lst)
