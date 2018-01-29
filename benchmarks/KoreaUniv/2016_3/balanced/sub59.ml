type mobile = branch * branch
and branch = SimpleBranch of length * weight
			|CompoundBranch of length * mobile
and length = int 
and weight = int 



let rec mobileweight m=
match m with
(l,r)->						let branchweight b = 
								match b with
								|SimpleBranch(_,w)->w
								|CompoundBranch(_,m)-> mobileweight m

								 in (branchweight l) + (branchweight r)

let rec balanced m =
match m with
(l,r)-> let branchbalanced b = 
			match b with 
			|SimpleBranch(_,_)->true
			|CompoundBranch(_,m1)->balanced m1
		in
			let torque b =	
			match b with 
			|SimpleBranch(l,w)->l*w
			|CompoundBranch(l,m)->l*(mobileweight m)
		in
			if ( torque l ) = (torque r ) && (branchbalanced l)=true && (branchbalanced r) = true then true
				else false 

  
