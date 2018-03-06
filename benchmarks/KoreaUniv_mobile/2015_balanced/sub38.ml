  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
let rec findweight : branch -> int
= fun b ->
match b with
|SimpleBranch (l,w) -> w
|CompoundBranch (l,(a,b)) -> findweight a + findweight b

let rec balanced : mobile -> bool
  =fun (lb,rb) -> 
match lb with
|CompoundBranch (l,(a,b))->
	if balanced (a,b) then
	(match rb with
	|CompoundBranch (m,(c,d))-> if balanced (c,d) then
					((l*(findweight lb)) = (m*(findweight rb))) else false
	|SimpleBranch (m,x)-> ((l*findweight lb)=(m*x))
	) else false
|SimpleBranch (l,w)->
	(match rb with
	|CompoundBranch (m,(c,d))-> if balanced (c,d) then
					((l*w) = (m*(findweight rb))) else false
	|SimpleBranch (m,x)-> ((l*w)=(m*x))
	)
