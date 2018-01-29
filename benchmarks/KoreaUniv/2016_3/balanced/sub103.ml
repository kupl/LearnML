
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun (mob) -> raise NotImplemented (* TODO *)

	let rec weight : branch -> weight
	= fun br -> 
		match br with
		SimpleBranch (l,w) -> w
		|CompoundBranch(l,mo) ->
			(match mo with
				(lb, rb) -> (weight lb) + (weight rb));;

	let rec ratio : branch -> int
	= fun br ->
		match br with
		SimpleBranch(l, w) -> l * w
		|CompoundBranch(l,mo) ->
			(match mo with
				(lb, rb) -> l * (weight lb + weight rb));;

   let rec balanced : mobile -> bool
   = fun (lb, rb)->
			match lb, rb with
				SimpleBranch (l1, w), SimpleBranch(l2, m) ->
					if(ratio lb = ratio rb) then true else false
   		  | SimpleBranch (l1,w),CompoundBranch (l2,m) ->
      		 if (balanced m = true)&&(ratio lb = ratio rb) then true else false
  			| CompoundBranch (l1,m),SimpleBranch (l2,w) ->
      		 if (balanced m = true)&&(ratio lb = ratio rb) then true else false
  			| CompoundBranch (l1,m1),CompoundBranch (l2,m2) ->
      		 if (balanced m1 = true) && (balanced m2 = true)&&(ratio rb= ratio lb) then true else false;;
