type mobile = branch * branch 
and branch = SimpleBranch of length * weight 
			|CompoundBranch of length * mobile 
and length = int 
and weight = int 

let rec findweight b 
 = match b with
|SimpleBranch(l,w) ->  w 
|CompoundBranch ( l , (f1,f2) ) -> (findweight f1 +findweight f2) 

 
let rec findvalue b 
= match b with
|SimpleBranch ( l , w ) -> l * w  
|CompoundBranch ( l , m ) -> l * ( findweight b )
 
let rec isbalanced m= match m with 
|(b1, b2) -> if ( (findvalue b1) = (findvalue b2) ) then true else false


let balanced m= match m with 
|(SimpleBranch (l1,w1), SimpleBranch(l2,w2) )  -> (isbalanced m) 
|(SimpleBranch (l1,w1), CompoundBranch(l2,m2) ) -> (isbalanced m) && (isbalanced m2)
|(CompoundBranch (l1,m1), SimpleBranch(l2,w2) ) -> (isbalanced m) && (isbalanced m1)
|(CompoundBranch (l1,m1), CompoundBranch(l2,m2)) -> (isbalanced m) && (isbalanced m1) && (isbalanced m2)

   