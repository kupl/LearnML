
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec addweight : mobile -> int 
	= fun subadd -> 
	match subadd with
  	| (SimpleBranch(l1,w), CompoundBranch(l2,m)) -> ((w) + addweight m)
  	| (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> (addweight m1 + addweight m2)
  	| (CompoundBranch(l1, m), SimpleBranch(l2, w)) -> (addweight m + (w))
  	| (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) -> (w1) + (w2)

  let rec balanced : mobile -> bool
  = fun mob -> 
  	match mob with
  	| (SimpleBranch(l1,w), CompoundBranch(l2,m)) -> if (balanced m = false) then false 
  													else if (l1 * w) = (l2 * addweight m) then true
  														else false
  	| (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> if (balanced m1 && balanced m2) then
  														if (l1*(addweight m1) = l2*(addweight m2)) then true else false
  													else false
  	| (CompoundBranch(l1, m), SimpleBranch(l2, w)) -> if (balanced m) then 
  													if ((l1*w) = l2*(addweight m)) then true else false
  													else false
  	| (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) -> if (l1 * w1) = (l2 * w2) then true else false
