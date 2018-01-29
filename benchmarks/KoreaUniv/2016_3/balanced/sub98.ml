
  	type mobile = branch * branch
  	and branch =
  	| SimpleBranch of length * weight
  	| CompoundBranch of length * mobile
  	and length = int
  	and weight = int
  
  	let rec sum : branch -> int
  	= fun w ->
  	match w with
  	| SimpleBranch(a, b) -> b
  	| CompoundBranch(a, (b, c)) -> (sum b) + (sum c);;

  
  	let rec multiple : branch -> int
  	= fun mobil ->
  	match mobil with
  	| SimpleBranch(a, b) -> a * (sum mobil)
  	| CompoundBranch(a, (b, c)) -> a * (sum mobil);;
  

  	let rec balanced : mobile -> bool
  	= fun mobil ->
  	let rec bal : branch -> bool
  	= fun branc ->
	match branc with
	| SimpleBranch(l, w) -> true
	| CompoundBranch(l, (w1, w2)) -> if (multiple w1) = (multiple w2) then (bal w1) && (bal w2) else false
	in
	match mobil with
	| (a, b) -> ((bal a) && (bal b)) && ((multiple a) = (multiple b))