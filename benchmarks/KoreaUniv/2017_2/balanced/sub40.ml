(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> let rec br(r,p,d) = match r with
|SimpleBranch(l,w) -> w * (l * d + p)
|CompoundBranch(l,m) -> (match m with
  |(a,b) -> (br(a,(p+l*d),-1) + br(b,(p+l*d),1))) in
if (let rec mob mb = match mb with
  |(a,b) -> br(a,0,-1) + br(b,0,1) in mob m)=0 then true else false