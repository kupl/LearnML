(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec measure_weight: mobile -> int
= fun m -> match m with
			| (SimpleBranch(l_length, l_weight), SimpleBranch(r_length, r_weight)) -> (l_weight + r_weight)
			| (SimpleBranch(l_length, l_weight), CompoundBranch(r_length, r_mobile)) -> (l_weight + (measure_weight r_mobile))
			| (CompoundBranch(l_length, l_mobile), SimpleBranch(r_length, r_weight)) -> ((measure_weight l_mobile) + r_weight)
			| (CompoundBranch(l_length, l_mobile), CompoundBranch(r_length, r_mobile)) -> (measure_weight l_mobile) + (measure_weight r_mobile);;

let rec balanced : mobile -> bool
= fun m -> match m with
			| (SimpleBranch(l_length, l_weight), SimpleBranch(r_length, r_weight)) -> if (l_length*l_weight- r_weight*r_length)=0 then true else false
			
			| (SimpleBranch(l_length, l_weight), CompoundBranch(r_length, r_mobile)) -> 
					if (balanced r_mobile) then 
					balanced ( 
						SimpleBranch(l_length,l_weight) , 
						SimpleBranch(r_length, (measure_weight  r_mobile)) )

					else false

			| (CompoundBranch(l_length, l_mobile), SimpleBranch(r_length, r_weight)) -> 
					if (balanced l_mobile) then
					balanced ( 
						SimpleBranch(l_length, (measure_weight l_mobile)) , 
						SimpleBranch(r_length,r_weight) )
					else false

			| (CompoundBranch(l_length, l_mobile), CompoundBranch(r_length, r_mobile)) -> 
					if (balanced l_mobile) then
						if (balanced r_mobile) then
					balanced ( 
						SimpleBranch(l_length, (measure_weight l_mobile)) , 
						SimpleBranch(r_length, (measure_weight r_mobile)) )
						else false
					else false;;
