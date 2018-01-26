
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec getWeight : mobile -> int
	= fun mo -> match mo with
	  				| SimpleBranch(a,b), SimpleBranch(c,d) -> b + d
	  				| SimpleBranch(a,b), CompoundBranch(c,d) -> b + (getWeight d)
     				| CompoundBranch(a,b), SimpleBranch(c,d) -> (getWeight b) + d
						| CompoundBranch(a,b), CompoundBranch(c,d) -> (getWeight b) + (getWeight d)

	let branchval : branch -> int 
	= fun br -> match br with
	| SimpleBranch (x, y) -> x * y
	| CompoundBranch (x, y) -> x * (getWeight y)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
	| (x,y) -> match x, y with
						| SimpleBranch(a,b), SimpleBranch(c,d) -> branchval x = branchval y
						| SimpleBranch(a,b), CompoundBranch(c,d) -> branchval x = branchval y && (balanced d)
						| CompoundBranch(a,b), SimpleBranch(c,d) -> branchval x = branchval y && (balanced b)
						| CompoundBranch(a,b), CompoundBranch(c,d) -> branchval x = branchval y && (balanced b) && (balanced d)
