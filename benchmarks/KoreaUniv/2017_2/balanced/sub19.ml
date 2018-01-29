(* problem 6*) 
type mobile = branch * branch 
(* left and rigth branches *) 
	and branch = 
	SimpleBranch of length * weight 
	| CompoundBranch of length * mobile 
	and length = int 
	and weight = int 
	
	let rec bweight : branch -> weight
  = fun brc ->
      match brc with
      SimpleBranch (_,w) -> w
      |CompoundBranch (l,m) ->
      (match m with
	     (right,left) -> (bweight right)+ (bweight left));;
	
	let rec btorque : branch -> int
	= fun brc ->
	match brc with 
	SimpleBranch (l,w) -> l * w
	|CompoundBranch (l,m) -> 
		(match m with
			(right,left) -> l * (btorque right * btorque left));;
	let rec balanced : mobile -> bool 
= fun m -> 
match m with 
(left,right) ->
(match left,right with 
|SimpleBranch (l1,w1),SimpleBranch (l2,w2) ->
if (btorque left = btorque right) then true else false
|SimpleBranch (l1,w1),CompoundBranch (l2,m) ->
if (balanced m = true) && (btorque left = btorque right) then true else false
|CompoundBranch (l1,m), SimpleBranch (l2,w2) ->
if (balanced m = true) && (btorque left = btorque right) then true else false
|CompoundBranch(l1,m1), CompoundBranch(l2,m2) ->
if (balanced m1 = true) && (balanced m2 = true) &&(btorque left = btorque right)
then true else false
)