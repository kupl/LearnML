  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int


  let rec makebranchweight : branch -> weight 
  =fun (b) ->
  match b with
  |SimpleBranch(l,w) -> w
  |CompoundBranch(l,w) -> let rec makemobileweight : mobile -> int
  =fun (m) ->
  match m with
  |(a,b) -> makebranchweight(a) + makebranchweight(b) in makemobileweight(w)

  let rec makemobileweight : mobile -> int
  =fun (m) ->
  match m with
  |(a,b) -> makebranchweight(a) + makebranchweight(b)

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb with 
  |SimpleBranch (l1,w1) ->(
		match rb with
		|SimpleBranch (l2,w2) ->
			if (l1*w1=l2*w2) then true else false
		|CompoundBranch (l2,w2) -> 
			if (balanced(w2)) then if (l1*w1=l2*makemobileweight(w2)) then true else false else false
				)
  |CompoundBranch (l1,w1) -> 
		(match rb with
		|SimpleBranch (l2,w2) ->
			if (balanced(w1)) then if (l2*w2=l1*makemobileweight(w1)) then true else false else false
		|CompoundBranch (l2,w2) -> 
  			if(balanced(w1)&&balanced(w2))  then if (l2*makemobileweight(w2)=l1*makemobileweight(w1)) then true else false else false
    )
