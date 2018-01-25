type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec sumW = 
  fun b ->
  match b with
  |SimpleBranch(l, w) -> w
  |CompoundBranch(len, mb) -> 
    match mb with
    | (lb,rb) -> sumW lb + sumW rb 

let rec balanced : mobile -> bool
=fun mb -> 
  match mb with
  |(lb, rb) -> 
    (match lb with
      |SimpleBranch(l1,w1) ->
        (match rb with
          |SimpleBranch(l2,w2) -> if sumW(lb)*l1 = sumW(rb)*l2
            then true else false
          |CompoundBranch(l, m) -> if (sumW(lb)*l1 = sumW(rb)*l)&&balanced(m)
            then true else false 
        )
      |CompoundBranch(l1,m1) ->
        (match rb with
          |SimpleBranch(l, w) -> if (sumW(lb)*l1 = sumW(rb)*l)&&balanced(m1)
            then true else false
          |CompoundBranch(l2, m2) -> if (sumW(lb)*l1 = sumW(rb)*l2)&&balanced(m1)&&balanced(m2)
            then true else false
        )   
    )
