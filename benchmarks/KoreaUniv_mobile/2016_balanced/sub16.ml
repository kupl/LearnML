
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob ->  let rec f mob = match mob with
		| (SimpleBranch (a,b) , SimpleBranch (c,d)) -> if (a*b)==(c*d) then true else false
		| (SimpleBranch (a,b) , CompoundBranch (c,d)) ->  let rec g d = match d with
						| (SimpleBranch (x,y) , SimpleBranch (z,t)) -> y+t
						| (SimpleBranch (x,y) , CompoundBranch (z,t)) -> if (f t) then y+(g t) else 100000000
						| (CompoundBranch (x,y) , SimpleBranch (z,t)) -> if (f y) then t+(g y) else 100000000
						| (CompoundBranch (x,y) , CompoundBranch (z,t))-> if (x*(g y))==(z*(g t)) then (g y)+(g t) else 10000000
					in if ((a*b)=(c*(g d)))&&(f d) then true else false
		| (CompoundBranch (a,b) , SimpleBranch (c,d)) ->  let rec g b = match b with
						| (SimpleBranch (x,y) , SimpleBranch (z,t)) -> y+t
						| (SimpleBranch (x,y) , CompoundBranch (z,t)) -> if (f t) then y+(g t) else 100000000
						| (CompoundBranch (x,y) , SimpleBranch (z,t)) -> if (f y) then t+(g y) else 100000000
						| (CompoundBranch (x,y) , CompoundBranch (z,t))-> if (x*(g y))==(z*(g t)) then (g y)+(g t) else 10000000
					in if ((c*d)=(a*(g b)))&&(f b) then true else false
		| (CompoundBranch (a,b) , CompoundBranch (c,d)) ->   let rec g b = match b with
						| (SimpleBranch (x,y) , SimpleBranch (z,t)) -> y+t
						| (SimpleBranch (x,y) , CompoundBranch (z,t)) -> if (f t) then y+(g t) else 100000000
						| (CompoundBranch (x,y) , SimpleBranch (z,t)) -> if (f y) then t+(g y) else 100000000
						| (CompoundBranch (x,y) , CompoundBranch (z,t))-> if (x*(g y))==(z*(g t)) then (g y)+(g t) else 10000000
					in if (a*(g b))=(c*(g d)) then true else false
	in f mob