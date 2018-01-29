
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let rec we : mobile ->int
	= fun mob -> match mob with
		|(b1,b2)-> 
		begin
			match b1,b2 with
			| ((SimpleBranch (l1,w1)),(SimpleBranch (l2,w2))) -> 
			if l1*w1=l2*w2 then w1+w2 else 0
			| ((SimpleBranch (l1,w1)),(CompoundBranch (l2,m))) ->
			if l1*w1= l2*(we m) then w1+(we m) else 0
			| ((CompoundBranch (l1,m)),(SimpleBranch (l2,w2))) ->
			if l1*(we m)=l2*w2 then (we m)+w2 else 0
			| ((CompoundBranch (l1,m1)),(CompoundBranch (l2,m2))) ->
			if (l1*(we m1)=l2*(we m2) && l1*(we m1)<>0) then (we m1)+(we m2) else 0
		end
  let balanced : mobile -> bool
  = fun mob -> if (we mob)=0 then false else true