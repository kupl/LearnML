type mobile = branch * branch
and branch = SimpleBranch of length * weight
		   | CompoundBranch of length * mobile
and length = int
and weight = int

(*if balnaced return (weight,len) if not weight is -1*)
							(* len & weight *)
let rec calweight : branch -> (int * int)
= fun br ->
	match br with
	| SimpleBranch (len, weight) -> (len, weight)
	| CompoundBranch (len, mob) ->
		(match mob with
		 | (br1, br2) ->
			let (ll, lw) = calweight(br1)
				in let (rl, rw) = calweight(br2)
					in if ll*lw = rl*rw then (len, lw+rw)
					  else (len, -1)
		)

let balanced : mobile -> bool
=fun (lb,rb) -> 
	let (len1, wei1) = (calweight lb)
		in let (len2, wei2) = calweight rb
			in if len1 * wei1 = len2 * wei2 then true
			   else false
