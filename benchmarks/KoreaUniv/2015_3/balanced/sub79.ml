  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec weight : mobile -> int
= fun (lb, rb) -> match (lb, rb) with
	 (SimpleBranch (ll, lw), SimpleBranch (rl, rw)) ->
		lw + rw
	| (SimpleBranch (ll, lw), CompoundBranch (rl, rm)) ->
		lw + (weight rm)
	| (CompoundBranch (ll, lm), SimpleBranch (rl, rw)) ->
		(weight lm) + rw
	| (CompoundBranch (ll, lm), CompoundBranch (rl, rm)) ->
		(weight lm) + (weight rm);;
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match (lb, rb) with
	(SimpleBranch (ll, lw), SimpleBranch (rl, rw)) ->
		(if (ll * lw) = (rl * rw) then
			true else false)
	| (SimpleBranch (ll, lw), CompoundBranch (rl, rm)) ->
		(if (ll * lw) = (rl * (weight rm)) && (balanced rm) then
			true else false)
	| (CompoundBranch (ll, lm), SimpleBranch (rl, rw)) ->
		(if (ll * (weight lm)) = (rl * rw) && (balanced lm) then
			true else false)
	| (CompoundBranch (ll, lm), CompoundBranch (rl, rm)) ->
		(if (ll * (weight lm)) = (rl * (weight rm)) && (balanced lm) &&
			(balanced rm) then
			true else false);;
