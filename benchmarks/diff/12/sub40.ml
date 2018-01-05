type ae =
	| CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec diff (ae, str) =
	match ae with
	| CONST i -> CONST 0
	| VAR var ->
		if var = str
			then CONST 1
			else CONST 0
	| POWER (var, pow) ->
		if var = str
			then
				if pow = 1
					then CONST 1
					else TIMES [CONST pow; POWER (var, pow-1)]
			else CONST 0
	| TIMES lst ->
		let rec diff_product left_elts right_elts =
			if right_elts = []
				then []
				else
					let hd = List.hd right_elts in
					let tl = List.tl right_elts in
					(TIMES (List.rev_append left_elts (diff (hd, str)::tl)))::( diff_product (hd::left_elts) tl )
		in
		SUM (diff_product [] lst)
	| SUM lst ->
		let rec diff_sum elts =
			if elts = []
				then []
				else
					let hd = List.hd elts in
					let tl = List.tl elts in
					(diff (hd, str))::(diff_sum tl)
		in
		SUM (diff_sum lst)
