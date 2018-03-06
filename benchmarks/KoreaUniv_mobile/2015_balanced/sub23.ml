
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec findweight : branch -> int
	=fun (branch) -> match branch with
									|SimpleBranch(a,b) -> b
									|CompoundBranch(a,(bl,br))-> findweight(bl) + findweight(br)



	let rec score : branch -> int
	=fun (branch) -> match branch with
								|SimpleBranch(a,b) -> a*b  (* length*weight *)
								|CompoundBranch(a,(bl,br)) -> a*(findweight(bl)+findweight(br))
								
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) ->match (lb,rb) with
	|(SimpleBranch(a,b),SimpleBranch(c,d)) -> score(lb) = score(rb)
	|(SimpleBranch(a,b),CompoundBranch(c,e)) -> (score(lb) = score(rb)) && balanced(e)
	|(CompoundBranch(c,e),SimpleBranch(a,b)) -> (score(lb) = score(rb)) && balanced(e)
	|(CompoundBranch(a,f),CompoundBranch(c,e)) -> (score(lb) = score(rb)) && (balanced(f) && balanced(e))
