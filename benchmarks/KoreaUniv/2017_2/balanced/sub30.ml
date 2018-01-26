(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> 
let rec weight : branch -> weight
= fun w -> match w with 
	| SimpleBranch (l, w) -> w
	| CompoundBranch (l, m) ->( begin
		match m with 
		| (left,right) -> (weight left) + (weight right)
	end)
in let calc_Torq : branch -> int
=fun torq -> begin
	match torq with
	| SimpleBranch (l, w) -> l*w
	| CompoundBranch (l, m) -> (match m with
		|(left, right) -> l*(weight left) + l*(weight right))
end
in match m with
| (left, right) -> (match left, right with
										| SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> if (calc_Torq left) = (calc_Torq right) then true else false
										| SimpleBranch (l1, w2), CompoundBranch (l2, m2) -> if (balanced m2 = true) && (calc_Torq left = calc_Torq right) then true else false
										| CompoundBranch (l1, m1), SimpleBranch (l2, w2) -> if (balanced m1 = true) && (calc_Torq left = calc_Torq left) then true else false
										| CompoundBranch (l1, m1), CompoundBranch (l2, m2) -> 
																								if (balanced m1 = true)  && (balanced m2 = true) && (calc_Torq left = calc_Torq right) then true else false);;