type ae = CONST of int
				| VAR of string
				| POWER of string * int
				| TIMES of ae list
				| SUM of ae list

exception InvalidArgument

let diff (ae,diff_by) = 
	let rec diff_inner (ae, diff_by) = 
		match ae with
		| CONST(i) -> 
			CONST(0)
		| VAR(v) -> 
			if v=diff_by then CONST(1) else CONST(0)
		| POWER(v,i) -> 
			if v=diff_by 
			then TIMES([CONST(i);POWER(v,(i-1))])
			else CONST(0)
		| TIMES(ae_list) ->
			if (List.length ae_list)=0 then raise InvalidArgument
			else
				let (_, res) = 
					List.fold_left (fun (idx,res) element ->
						let (_, times) =
							List.fold_left (fun (times_idx, times) element -> 
								if times_idx=idx then (times_idx+1, times)
								else (times_idx+1, element::times)
							) (0, []) ae_list
						in

						let ae_mid_res = diff_inner(element, diff_by)::times in
						(idx+1, TIMES(ae_mid_res)::res)
					) (0, []) ae_list
				in
				SUM(res)
		| SUM(ae_list) -> 
			if (List.length ae_list)=0 then raise InvalidArgument 
			else 
				let res = 
					List.fold_left (fun res element ->
						(diff_inner(element, diff_by))::res
					) [] ae_list
				in
				SUM(res)
	in

	diff_inner(ae, diff_by)

