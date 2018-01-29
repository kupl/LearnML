(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m ->
 let rec weightsum x=match x with
  |SimpleBranch (l,w)->w
  |CompoundBranch (l,n)->let (b,c)=n in weightsum b + weightsum c
 in let rec mobilesum n= let (b,c)=n in weightsum b + weightsum c
 in let torquesum x=match x with
  |SimpleBranch (l,w)->l*w
  |CompoundBranch (l,n)->l*mobilesum n
 in let det x=match x with
  |SimpleBranch (y,z)->true
  |CompoundBranch (y,z)->balanced z
 in let (p,q)=m in if torquesum p=torquesum q then det p && det q else false