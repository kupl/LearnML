  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  

  let rec weightcheck : mobile -> int
  =fun (lb, rb) -> match lb, rb with
				   | SimpleBranch (al, aw), SimpleBranch (bl, bw) -> aw + bw 												  				  	
			       | SimpleBranch (al, aw), CompoundBranch (bl, (mob1, mob2)) -> aw + weightcheck(mob1, mob2)
			       | CompoundBranch (al, (mob1, mob2)), SimpleBranch (bl, bw) -> weightcheck(mob1, mob2) + bw
			       | CompoundBranch (al, (moba1, moba2)), CompoundBranch(bl, (mobb1, mobb2)) -> weightcheck(moba1, moba2) + weightcheck(mobb1, mobb2)

  let rec balanced : mobile -> bool
  =fun (lb, rb) -> match lb, rb with
				   | SimpleBranch (al, aw), SimpleBranch (bl, bw) -> if al*aw = bl*bw then true
				    											  	 else false
				   | SimpleBranch (al, aw), CompoundBranch (bl, (mob1, mob2)) -> if balanced((mob1, mob2))&&(al*aw = bl*weightcheck(mob1, mob2)) then true
				 															  	 else false
				   | CompoundBranch (al, (mob1, mob2)), SimpleBranch (bl, bw) -> if balanced(mob1, mob2)&& (al*weightcheck(mob1, mob2) = bl*bw) then true
				 															  	 else false
				   | CompoundBranch (al, (moba1, moba2)), CompoundBranch (bl, (mobb1, mobb2)) -> if balanced(moba1, moba2)&&balanced(mobb1, mobb2)&& (al*weightcheck(moba1, moba2) = bl*weightcheck(mobb1, mobb2)) then true
				  																			     else false
