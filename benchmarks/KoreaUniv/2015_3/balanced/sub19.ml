  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let leng m = match m with
	|SimpleBranch (left, right) -> left
	|CompoundBranch (left, right) -> left

  let rec weight m = match m with
	|SimpleBranch (left, right) -> right
	|CompoundBranch (left, right) -> let mob k = match k with
						|(left,right) -> (weight left) + (weight right) in mob right
 
  let balanced : mobile -> bool = fun m -> match m with
	|(left, right) -> if ( (leng left * weight left) = (leng right * weight right) ) then true else false
 