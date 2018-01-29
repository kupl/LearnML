
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

(*  let balanced : mobile -> bool
  = fun mob -> true;;
*)
	let rec multi n i
	= if(i=0) then 0 else n + (multi n (i-1));;

	let rec caltorque : mobile -> int
	= fun mob ->
			(match mob with
			|(SimpleBranch(l1,w1), CompoundBranch(l2,m))
					->(w1) + (caltorque m)
			|(SimpleBranch(l1,w1), SimpleBranch(l2,w2))
					-> w1 + w2
			|(CompoundBranch(l1,m1),CompoundBranch(l2,m2))
					->(caltorque m1)+(caltorque m2)
			|(CompoundBranch(l1,m), SimpleBranch(l2,w))
					->(caltorque m)+(w));;

	let rec balanced2 : mobile -> bool
	= fun mob ->
		(match mob with
		|(SimpleBranch(l1,w1), CompoundBranch(l2,m))
				->if (((multi l1 w1) = multi l2 (caltorque m))&&(balanced2 m))
							then true else false
		|(CompoundBranch(l1,m), CompoundBranch(l2,m2))
				->if (((multi l1 (caltorque m)) = (multi l2 (caltorque m2)))&& (balanced2 m)&&(balanced2 m2))
							then true else false
		|(SimpleBranch(l1,w1), SimpleBranch(l2,w2))
				->if (multi l1 w1) = (multi l2 w2)
							then true else false
		|(CompoundBranch(l1,m), SimpleBranch(l2,w1))
				->if (((multi l1 (caltorque m)) = (multi l2 w1))&&(balanced2 m))
							then true else false);;
	let balanced : mobile -> bool
	= fun mob -> balanced2 mob;;
