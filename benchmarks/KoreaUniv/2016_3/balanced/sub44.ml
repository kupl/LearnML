
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec balanced : mobile -> bool
= fun mob -> match mob with
    | (a,b) -> if getTorque a 0 0 + getTorque b 0 1 = 0 then true else false
and getTorque : branch -> int -> int -> int
= fun br len dir -> match br with
    | SimpleBranch (a,b) -> if dir = 0 then (len + a)*b else (len - a)*b
    | CompoundBranch (length,(l,r)) -> if balanced (l,r) = true then (getTorque l (length+len) 0) + (getTorque r (len+length) 1) else -9999999999
