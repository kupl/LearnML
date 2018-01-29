(* problem 6*)
type mobile = branch * branch    (*left and rihgt branches*)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let lft tuple = 
  match tuple with
  (x,y)->x
let rgt tuple =
  match tuple with
  (x,y)-> y

let rec wgt branch =
  match branch with
  SimpleBranch(x,y)-> y
  |CompoundBranch(x,y)-> (wgt (lft y))+(wgt (rgt y))

let toq exp =
  match exp with
  |SimpleBranch(n1,n2)-> n1*n2
  |CompoundBranch(n,branch)-> n*((wgt (lft branch)) + (wgt(rgt branch)))

let rec bal exp =
  match exp with
  (x,y)->
  if toq x = toq y then
  (match x,y with
    CompoundBranch(n1,b1) , CompoundBranch(n2,b2)-> (bal b1)&&(bal b2)
    |CompoundBranch(n1,b1), SimpleBranch(k1,k2) -> bal b1
    |SimpleBranch(k1,k2), CompoundBranch(n1,b1) -> bal b1
    |SimpleBranch(k1,k2), SimpleBranch(k3,k4) -> true)
  else false



let balanced : mobile -> bool
= fun m ->
let v0 = bal m in
match v0 with
true-> true
|false-> false
