

type mobile = branch * branch
and branch = SimpleBranch of length * weight
	    |CompoundBranch of length * mobile
and length = int
and weight = int

let rec rweight m = match m with
| (SimpleBranch (x,y), SimpleBranch (a,b)) -> if (x*y) = (a*b) then y*b else -1
| (SimpleBranch (x,y), CompoundBranch (a,b)) -> if (x*y)=(a*(rweight b)) then y+(rweight b) else -1
| (CompoundBranch (x,y), SimpleBranch (a,b)) -> if (x*(rweight y)) = (a*b) then (rweight y) + b else -1
| (CompoundBranch (x,y), CompoundBranch (a,b)) -> if (x*(rweight y))=(a*(rweight b)) then ((rweight y)+(rweight b)) else -1

let rec balanced f = 
match f with
| (SimpleBranch (x,y), SimpleBranch(a,b)) -> if (x*y) =(a*b) then true else false
| (SimpleBranch (x,y), CompoundBranch (a,b)) -> if (x*y) = (a*(rweight b)) then true else false
| (CompoundBranch (x,y), SimpleBranch (a,b)) -> if (x*(rweight y)) = (a*b) then true else false
| (CompoundBranch (x,y), CompoundBranch (a,b)) -> if (x*(rweight y)) = (a*(rweight b)) then true else false;;
