  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
let balanced : mobile -> bool
= fun(lb,rb) ->
 in let cal(lb, rb) =
  in let x =
 match lb with
  |CompoundBranch -> cal(lb.mobile.lb, lb.mobile.rb)
  |SimpleBranch -> lb.length*lb.weight  
  in let y =
 match rb with
  |CompoundBranch -> cal(rb.mobile.lb, rb.mobile.rb)
  |SimpleBranch -> rb.length*rb.weight 
 if x - y = 0 then true