(* ------------------problem6------------------ *)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> (* TODO *)
	let rec getW b =
		match b with
		| SimpleBranch(len,wgt) -> wgt
		| CompoundBranch(len, mob) -> match mob with (l, r) -> (getW l) +(getW r) in
	let rec cal b =
		match b with
		| SimpleBranch(len, wgt) -> len*wgt
		| CompoundBranch(len, mob) -> if not (balanced mob) then (-100)
									else match mob with (l, r) -> len*((getW l)+(getW r)) in
	match m with
	|(left, right) -> if((cal left)= (-100) || (cal right)= (-100)) then false
					  else (cal left)=(cal right);;
					  (*양쪽이 unbalanced일때 가능한지 체크하기 : 판별가능 -> 그대로 두기
														 :안됨 -> 주석 풀고, 다시 체크*)