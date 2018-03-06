type mobile = branch * branch 
and branch = SimpleBranch of length * weight 
        |CompoundBranch of length * mobile 
and length = int 
and weight = int 
let balanced : mobile -> bool = fun (lb,rb) -> false 
 
let rec sumWeight b =
match b with
|SimpleBranch(le, we) ->  we 
|CompoundBranch (l,(lb,rb)) -> (sumWeight lb + sumWeight rb) 
 
let rec multiple b = 
match b with
|SimpleBranch (le, we) -> le * we  
|CompoundBranch (le, m) -> le * (sumWeight b)
 
let rec compare m = 
match m with 
|(lb, rb)-> 
   if((multiple lb) = (multiple rb)) then true 
   else false
  
let balanced m = 
match m with 
|(SimpleBranch (le1,we1), SimpleBranch(le2,we2))  -> compare m 
|(SimpleBranch (le1,we1), CompoundBranch(le2,m2)) -> (compare m) && (compare m2)
|(CompoundBranch (le1,m1), SimpleBranch(le2,we2) ) -> (compare m) && (compare m1)
|(CompoundBranch (le1,m1), CompoundBranch(le2,m2)) -> (compare m) && (compare m1) && (compare m2)

