
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec branch_weight : branch -> int
  = fun br ->
  	match br with
  		| SimpleBranch (len, wei) ->
  			wei
  		| CompoundBranch (len, (br1 , br2)) ->
			(branch_weight br1) + (branch_weight br2)
			
  let mobile_weight : mobile -> int
  = fun mob ->
  	match mob with
  		| br1, br2 ->
  			(branch_weight br1) + (branch_weight br2)

  let rec balanced : mobile -> bool
  = fun mob ->
  	match mob with
  		| CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2) ->
				(balanced mobile1) && (balanced mobile2) && (((mobile_weight mobile1)*length1) = ((mobile_weight mobile2)*length2))
  		| CompoundBranch(length1, mobile1), SimpleBranch(length2, weight2) ->
				(balanced mobile1) && (((mobile_weight mobile1)*length1) = (weight2*length2))
  		| SimpleBranch(length1, weight1), CompoundBranch(length2, mobile2) ->
				(balanced mobile2) && ((weight1*length1) = ((mobile_weight mobile2)*length2))
  		| SimpleBranch(length1, weight1), SimpleBranch(length2, weight2) ->
				((weight1*length1) = (weight2*length2))
				