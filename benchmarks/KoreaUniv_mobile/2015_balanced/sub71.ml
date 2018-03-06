type mobile = branch * branch
and branch = SimpleBranch of length * weight
			| CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced mobile =
	match mobile with
		(SimpleBranch(simLen1,simWei1),SimpleBranch(simLen2,simWei2)) -> if (simLen1*simWei1) = (simLen2*simWei2) then true else false
		|(SimpleBranch(simLen1,simWei1),CompoundBranch(compLen1,compMob1)) -> if balanced compMob1 = false then false else if 
																				(simLen1*simWei1) = mobWeightR(compMob1,compLen1) then true
																				else false
		|(CompoundBranch(compLen1,compMob1),SimpleBranch(simLen1,simWei1)) -> if balanced compMob1 = false then false else if
																				mobWeightL(compMob1,compLen1) = simLen1*simWei1 then true else false
		|(CompoundBranch(compLen1,compMob1),CompoundBranch(compLen2,compMob2)) -> if balanced compMob1 = false then false 
																				else if balanced compMob2 = false then false else
																				if mobWeightL(compMob1,compLen1)=mobWeightR(compMob2,compLen2) then true
																				else false
and mobWeightL =
	fun (mobile,length)->  
	match mobile with
		(SimpleBranch(simLen1,simWei1),SimpleBranch(simLen2,simWei2))-> (length+simLen1)*simWei1 + (length-simLen2)*simWei2
		|(SimpleBranch(simLen1,simWei1),CompoundBranch(compLen1,compMob1)) -> (length+simLen1)*simWei1 + mobWeightR(compMob1,length - compLen1)
		|(CompoundBranch(compLen1,compMob1),SimpleBranch(simLen1,simWei1)) -> mobWeightL (compMob1,length+compLen1) + (length-simLen1)*simWei1
		|(CompoundBranch(compLen1,compMob1),CompoundBranch(compLen2,compMob2))-> mobWeightL(compMob1,length+compLen1) + 
																		mobWeightR(compMob2,length-compLen2)
and mobWeightR = 
	fun (mobile,length)->
	match mobile with
		(SimpleBranch(simLen1,simWei1),SimpleBranch(simLen2,simWei2))->(length-simLen1)*simWei1 + (length+simLen2)*simWei2
		|(SimpleBranch(simLen1,simWei1),CompoundBranch(compLen1,compMob1)) -> (length-simLen1)*simWei1 + mobWeightR (compMob1,length + compLen1)
		|(CompoundBranch(compLen1,compMob1),SimpleBranch(simLen1,simWei1)) -> mobWeightL (compMob1,length-compLen1) + (length+simLen1)*simWei1
		|(CompoundBranch(compLen1,compMob1),CompoundBranch(compLen2,compMob2))-> mobWeightL(compMob1,length-compLen1) +
																		mobWeightR(compMob2,length+compLen2)
