
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob -> let rec subbalanced m=match m with 
				|(SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if l1*w1=l2*w2 then (true,w1+w2) else (false,0)
				|(SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> (match subbalanced(m2)with
																|(true,w2)-> if(l1*w1=l2*w2) then (true,w1+w2) else (false,0)
																|_->(false,0) )
				|(CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> ( match  subbalanced(m1)with 
																|(true,w1)-> if( l1*w1=l2*w2) then (true,w1+w2) else (false,0)
																|_->(false,0))
				|(CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> (match subbalanced(m1),subbalanced(m2) with
																|(true,w1),(true,w2)->if( l1*w1=l2*w2) then (true,w1+w2) else (false,0)
																|_->(false,0))
			in (match subbalanced mob with |(true,x)-> true
											|_-> false	)													
			
  