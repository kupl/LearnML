
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec scale : branch -> int
	= fun br -> match br with
	| SimpleBranch(len, w) -> len*w
	| CompoundBranch(len, (x, y)) -> len*(scale(x)+scale(y))

  let balanced : mobile -> bool
  = fun mob -> 
	let rec balance : mobile -> bool
	= fun mob -> match mob with
	|(SimpleBranch(a, b), SimpleBranch(c, d)) -> if a*b = c*d then true else false
	|(CompoundBranch(a, (p ,q)), SimpleBranch(c, d)) -> if a*(scale(p)+scale(q)) = c*d 
		then true&&balance(p, q) else false
	|(SimpleBranch(a, b), CompoundBranch(c, (p, q))) -> if a*b = c*(scale(p)+scale(q)) 
		then true&&balance(p, q) else false
	|(CompoundBranch(a, (x, y)), CompoundBranch(b, (p, q))) -> 
		if a*(scale(x)+scale(y)) = b*(scale(p)+scale(q)) 
		then balance(x, y)&&balance(p, q) else false
	in balance(mob)
	 