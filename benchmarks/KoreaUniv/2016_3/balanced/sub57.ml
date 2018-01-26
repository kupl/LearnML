
  type mobile = branch * branch
	  and branch = 
	  | SimpleBranch of length * weight
	  | CompoundBranch of length * mobile
	  and length = int
	  and weight = int
	;;

	let rec branchWeight : branch -> int
	  = fun b ->
	    match b with
	    | SimpleBranch(l, w) -> w
	    | CompoundBranch(l, m) ->
	      begin
	        match m with
	        | (b1, b2) -> branchWeight(b1) + branchWeight(b2)
	      end
	;;

	let rec mobileWeight : mobile -> int
	  = fun mob ->
	    match mob with
	    | (b1, b2) -> branchWeight(b1) + branchWeight(b2)
	;;

	let rec balanced : mobile -> bool
	  = fun mob ->
	    match mob with
	    | SimpleBranch(l1, w1), SimpleBranch(l2, w2) ->
	      if l1 * w1 = l2 * w2
	      then true
	      else false
	    | SimpleBranch(l1, w1), CompoundBranch(l2, m) ->
	      begin
	        balanced(m) && (l1 * w1 = l2 * mobileWeight(m))
	      end
	    | CompoundBranch(l1, m), SimpleBranch(l2, w2) ->
	      begin
	        balanced(m) && (l2 * w2 = l1 * mobileWeight(m))
	      end
	    | CompoundBranch(l1, m1), CompoundBranch(l2, m2) ->
	      begin
	        balanced(m1) && balanced(m2) && (l1 * mobileWeight(m1) = l2 * mobileWeight(m2))
	      end
	;;