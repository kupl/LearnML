(*  Problem2  *)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
            | CompoundBranch of length * mobile
and length = int
and weight = int

let rec returnweight c = match c with
| (SimpleBranch (x,y), SimpleBranch (a,b)) -> if (x*y)=(a*b) then y+b else -1
| (SimpleBranch (x,y), CompoundBranch (a,b)) -> if (x*y)=(a*(returnweight b)) then y+(returnweight b) else -1
| (CompoundBranch (x,y), SimpleBranch (a,b)) -> if (x*(returnweight y))=(a*b) then (returnweight y)+b else -1
| (CompoundBranch (x,y), CompoundBranch (a,b)) -> if (x*(returnweight y))=(a*(returnweight b)) then ((returnweight y)+(returnweight b)) else -1

let rec balanced : mobile -> bool = fun f -> match f with
| (SimpleBranch (x,y), SimpleBranch (a,b)) -> if (x*y)=(a*b) then true else false
| (SimpleBranch (x,y), CompoundBranch (a,b)) -> if (x*y)=(a*(returnweight b)) then true else false
| (CompoundBranch (x,y), SimpleBranch (a,b)) -> if (x*(returnweight y))=(a*b) then true else false
| (CompoundBranch (x,y), CompoundBranch (a,b)) -> if (x*(returnweight y))=(a*(returnweight b)) then true else false;;

 