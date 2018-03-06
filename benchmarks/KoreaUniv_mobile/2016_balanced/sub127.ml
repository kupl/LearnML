
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec f : mobile->int
  = fun mob -> 
  	match mob with 
  		| (SimpleBranch(a,b),SimpleBranch(c,d)) -> (b+d)
  		| (SimpleBranch(a,b), CompoundBranch(c,d)) -> (b+(f d))
  		| (CompoundBranch(a,b),SimpleBranch(c,d)) -> ((f b)+d)
  		| (CompoundBranch(a,b),CompoundBranch(c,d)) -> ((f b)+(f d))

  let rec balanced : mobile -> bool
  = fun mob -> 
  	match mob with 
  		| (SimpleBranch(a,b),SimpleBranch(c,d)) -> (a*b) = (c*d)
  		| (SimpleBranch(a,b), CompoundBranch(c,d)) ->((a*b) = (c*(f d)))&&(balanced d)
  		| (CompoundBranch(a,b),SimpleBranch(c,d)) -> ((a*(f b)) = (c*d))&&(balanced b)
  		| (CompoundBranch(a,b),CompoundBranch(c,d)) ->((a*(f b)) = (c*(f d)))&&(balanced b)&&(balanced d)
