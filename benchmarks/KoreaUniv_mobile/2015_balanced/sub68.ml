  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec branchweight br =
  	let mobileweight = fun (lb,rb) -> branchweight(lb)+branchweight(rb) in
  	match br with
  	| SimpleBranch(le,we) -> we
  	| CompoundBranch(le, mo) -> mobileweight(mo)

  let mobileweight = fun (lb,rb) -> branchweight(lb)+branchweight(rb)
  
  let branchbalance br = 
  	match br with
  	| SimpleBranch(le,we) -> le*we
  	| CompoundBranch(le,mo) -> le*mobileweight(mo)

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> if (branchbalance lb)=(branchbalance rb) then
  	(match lb with
  	| SimpleBranch(le,we) ->
  		(match rb with
  		| SimpleBranch(le2,we2) -> true
  		| CompoundBranch(le2, mo) -> balanced mo)
  	| CompoundBranch(le,mo) ->
  		(match rb with
  		| SimpleBranch(le2,we) -> balanced mo
  		| CompoundBranch(le2,mo2) -> if balanced mo then balanced mo2 else false))
  else false
