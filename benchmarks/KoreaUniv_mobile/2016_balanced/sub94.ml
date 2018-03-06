
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec weight : mobile -> int
  = fun mob -> match mob with
  |(SimpleBranch(a,b),SimpleBranch(c,d))->b+d
	|(SimpleBranch(a,b),CompoundBranch(c,d))->b+(weight d)
	|(CompoundBranch(a,b),SimpleBranch(c,d))->(weight b)+d
	|(CompoundBranch(a,b),CompoundBranch(c,d))->(weight b)+(weight d)

  let rec  balanced : mobile -> bool
  = fun mob -> match mob with
  |(SimpleBranch(a,b),SimpleBranch(c,d)) -> if a*b = c*d then true else false
  |(SimpleBranch(a,b),CompoundBranch(c,d))-> if (balanced d)=true then if a*b=c*(weight d) then true else false 
																					   else false 
  |(CompoundBranch(a,b),SimpleBranch(c,d)) -> if (balanced b)=true then if a*(weight b)=c*d then true else false
																						 else false 
  |(CompoundBranch(a,b),CompoundBranch(c,d)) -> if (balanced b)&&(balanced d)=true then if a*(weight b)=c*(weight d) then true else false
																						 else false  