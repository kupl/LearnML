  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
let rec totalweight : mobile -> int
=fun(lb,rb) ->
match lb with
|SimpleBranch(l,w) ->
  (match rb with
  |SimpleBranch(l2,w2) -> w + w2
  |CompoundBranch(l2,mob) -> w + (totalweight mob))
|CompoundBranch(l,mob) ->
  (match rb with
  |SimpleBranch(l2,w2)-> (totalweight mob)+ w2
  |CompoundBranch(l2, mob2) -> (totalweight mob) + (totalweight mob2))


let rec balanced : mobile -> bool
=fun (lb,rb) -> 
  match lb with
  |SimpleBranch(len_l,wei_l) ->
    (match rb with
    |SimpleBranch(len_r,wei_r) -> if len_l*wei_l = len_r*wei_r then true else false
    |CompoundBranch(len_r,mob) -> if len_r*(totalweight mob) = len_l*wei_l then balanced mob else false)
  |CompoundBranch(len_l,mob) ->
    (match rb with
    |SimpleBranch(len_r,wei_r) -> if len_l*(totalweight mob) = len_r*wei_r then balanced mob else false
    |CompoundBranch(len_r,mob2) -> if len_l*(totalweight mob) = len_r*(totalweight mob2) then (balanced mob)&&(balanced mob2) else false)

