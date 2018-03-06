(*Problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
            |CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight : mobile -> int
= fun m -> match m with
(lb,rb) ->
match lb with
|SimpleBranch(l1,w1) -> (match rb with
    |SimpleBranch(l2,w2)-> w1 + w2
    |CompoundBranch(l2,m2) -> w1 + (weight m2)
    )
|CompoundBranch(l3,m3) -> match rb with
    |SimpleBranch(l4,w4) -> (weight m3) + w4
    |CompoundBranch(l4,m4) -> (weight m3) + (weight m4)



let rec balanced : mobile -> bool
= fun m -> match m with
(lb,rb) -> 
match lb with
|SimpleBranch(l1,w1) -> (match rb with
    |SimpleBranch(l2,w2) -> if(l1*w1 = l2*w2) then true else false
    |CompoundBranch(l2,m2) -> if((balanced m2) = true) then (if(l1*w1 = l2*(weight m2)) then true else false) else false
    )

|CompoundBranch(l3,m3) -> if((balanced m3) = true) then (match rb with
    |SimpleBranch(l4,w4) -> if(l4*w4 = l3*(weight m3)) then true else false
    |CompoundBranch(l4,m4) -> if((balanced m4) = true) then (if(l3*(weight m3) = l4*(weight m4)) then true else false) else false
    )
  else false
