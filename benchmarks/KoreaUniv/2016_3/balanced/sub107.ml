
type mobile = branch * branch
and branch =
| SimpleBranch of length * weight
| CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
	= fun mob ->
	match mob with
	| SimpleBranch (lblen, lbwei), SimpleBranch (rblen, rbwei) 
		-> if lblen*lbwei = rblen*rbwei then true	else false
	| CompoundBranch (cblen, cmob), SimpleBranch (rblen, rbwei) 
		-> balanced cmob
	| SimpleBranch (lblen, lbwei), CompoundBranch (cblen, cmob) 
		-> balanced cmob
	| CompoundBranch (cblen1, cmob1), CompoundBranch(cblen2, cmob2)
  	-> balanced cmob1 && balanced cmob2