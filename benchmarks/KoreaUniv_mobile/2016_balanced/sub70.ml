
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec branchweight : mobile -> int
  = fun mob -> match mob with
	    (SimpleBranch (al,aw), SimpleBranch (bl, bw))
        -> aw + bw
    | (SimpleBranch (al,aw), CompoundBranch (bl,bm))
				-> aw + branchweight bm
		| (CompoundBranch (al,am), SimpleBranch (bl,bw))
				-> branchweight am + bw
		| (CompoundBranch (al,am), CompoundBranch (bl,bm))
        -> branchweight am + branchweight bm;;

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
		  (SimpleBranch (al,aw), SimpleBranch (bl,bw)) ->
			  if al*aw = bl*bw then true
			  else false
	  | (CompoundBranch (al,am), SimpleBranch (bl,bw)) ->
				if balanced am && al*branchweight am = bl*bw then true
				else false
		| (SimpleBranch (al,aw), CompoundBranch (bl,bm)) ->
				if balanced bm && al*aw = bl*branchweight bm then true
				else false
		| (CompoundBranch (al,am), CompoundBranch (bl,bm)) ->
				if balanced am && balanced bm 
           && al*branchweight am = bl* branchweight bm then true
				else false;; 
