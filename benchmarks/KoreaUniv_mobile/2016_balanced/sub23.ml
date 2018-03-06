
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
	and weight = int

	let rec sumweight : mobile -> int
	= fun mob -> 
match mob with
(SimpleBranch (_,a) , SimpleBranch(_,b)) -> a + b
|(SimpleBranch (_,a), CompoundBranch (_,b)) -> a + sumweight b
|(CompoundBranch(_,a), SimpleBranch(_,b)) -> sumweight a + b
| (CompoundBranch(_,a), CompoundBranch(_,b)) -> sumweight a + sumweight b
  let rec  balanced : mobile -> bool
  = fun mob ->
match mob with
(SimpleBranch (a,b) , SimpleBranch (c,d)) -> if a*b = c*d then true else false
|(SimpleBranch (a,b) , CompoundBranch (c,d)) -> if a*b = c * (sumweight d) && balanced d then true else false
|(CompoundBranch (a,b) , SimpleBranch (c,d)) -> if a*(sumweight b) = c * d && balanced b then true else false
|(CompoundBranch(a,b) , CompoundBranch (c,d)) -> if a * (sumweight b) = c * (sumweight d) && balanced b && balanced d then true else false (* TODO *)
