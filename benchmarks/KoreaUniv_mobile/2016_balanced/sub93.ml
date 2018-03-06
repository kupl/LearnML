
	type mobile=branch*branch
	and branch=
		|SimpleBranch of length*weight
		|CompoundBranch of length*mobile
	and length=int
	and weight=int
	let rec weight_ :branch->int
	=fun l-> 
	match l with
		|SimpleBranch(l1,w1)->w1
		|CompoundBranch(l1,(b1,b2))-> weight_(b1)+weight_(b2)
	
	let rec balanced : mobile->bool
	=fun mob->
	match mob with
		|SimpleBranch(l1,w1),SimpleBranch(l2,w2)->if (l1*w1=l2*w2) then true else false
		|SimpleBranch(l1,w1),CompoundBranch(l2,m) | CompoundBranch(l2,m),SimpleBranch(l1,w1)-> (balanced (m)) && (l1*w1=l2*(weight_ (CompoundBranch(l2,m))))
		|CompoundBranch(l1,m1), CompoundBranch(l2,m2)-> (balanced (m1)) &&(balanced (m2)) && (l1*(weight_(CompoundBranch(l1,m1)))=l2*(weight_(CompoundBranch(l2,m2))))
