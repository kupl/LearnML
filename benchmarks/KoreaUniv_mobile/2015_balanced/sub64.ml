type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec calpweight br =
		match br with
		SimpleBranch (l,w) -> w |
		CompoundBranch (l,m) ->
				(match m with
				(a,b) -> (calpweight a) + (calpweight b));;

let rec calweight br =
		match br with
		SimpleBranch (l,w) -> l*w |
		CompoundBranch (l,m) ->
				(match m with
				(a,b) -> if (calweight a)=(calweight b) then (((calpweight a)+(calpweight b))*l) else (-1));;

let balanced : mobile -> bool
=fun (lb,rb) ->
		 if ((calweight lb)=(calweight rb)) then true else false;;
