  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
	
  let rec totweight : mobile -> int
	=fun (lb,rb) -> 
	match lb with
	|SimpleBranch(l,w) -> 
		(match rb with
		|SimpleBranch(l2,w2) -> w + w2
		|CompoundBranch(l2,m) -> w + (totweight m))
	|CompoundBranch(l,m) ->
		(match rb with
		|SimpleBranch(l2,w2) -> (totweight m) + w2
		|CompoundBranch(l2,m2) -> (totweight m) + (totweight m2))


  let rec balanced : mobile -> bool
	=fun (lb,rb) -> 
  	match lb with
 	|SimpleBranch(len_l,wei_l) ->
   		(match rb with
    		|SimpleBranch(len_r,wei_r) -> if len_l*wei_l = len_r*wei_r then true else false
    		|CompoundBranch(len_r,m) -> if len_r*(totweight m) = len_l*wei_l then balanced m else false)
  	|CompoundBranch(len_l,m) ->
  		(match rb with
   		 |SimpleBranch(len_r,wei_r) -> if len_l*(totweight m) = len_r*wei_r then balanced m else false
   		 |CompoundBranch(len_r,m2) -> if len_l*(totweight m) = len_r*(totweight m2) then (balanced m)&&(balanced m2) else false)

