
(* problem 6*) 
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool = fun m 
-> let rec tweight : mobile -> int = fun m 
-> match m with 
| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1+w2
| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) -> (tweight w1)+ w2
| (SimpleBranch (l1, w1), CompoundBranch(l2, w2)) -> w1+(tweight w2)
| (CompoundBranch (l1, w1), CompoundBranch(l2,w2)) -> (tweight w1) + (tweight w2)in 

let rec balance : mobile->bool->bool = fun m b 
-> if b=false then false 
else match m with
| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if (l1*w1) = (l2*w2) then true else false
| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) -> if (balance w1 true) = false then false 
       else if ((tweight w1)*l1) = (l2*w2) then true
       else false 
| (SimpleBranch (l1, w1), CompoundBranch(l2, w2)) -> if (balance w2 true) = false then false 
       else if ((tweight w2)*l2) = (l1*w1) then true
       else false 
| (CompoundBranch (l1, w1), CompoundBranch(l2,w2)) -> if (balance w2 true)&&(balance w1 true) = false then false 
       else if ((tweight w2)*l2) = ((tweight w1)*l1) then true
       else false in 
balance m true
