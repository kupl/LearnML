  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec weight x=
	match x with 
	|(SimpleBranch(a, b), SimpleBranch(c,d)) -> b+d
	|(SimpleBranch(a, b), CompoundBranch(c,d)) -> b+(weight d)
	|(CompoundBranch(a, b), SimpleBranch(c,d)) -> (weight b) + d
	|(CompoundBranch(a, b), CompoundBranch(c,d)) -> (weight b)+(weight d)

  let rec balanced : mobile -> bool
  = fun mob ->
	match mob with 
	|(SimpleBranch(a,b), SimpleBranch(c,d)) -> if a*b=c*d then true else false
	|(SimpleBranch(a,b), CompoundBranch(c,d)) -> if (a*b=c*(weight d)) && balanced d then true else false
	|(CompoundBranch(a,b), SimpleBranch(c,d)) -> if (a*(weight b)=c*d) && balanced b then true else false
	|(CompoundBranch(a,b), CompoundBranch(c,d)) -> if (a*(weight b)=c*(weight d)) && balanced b && balanced d then true else false
