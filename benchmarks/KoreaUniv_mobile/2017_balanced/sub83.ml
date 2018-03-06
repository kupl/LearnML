(*problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->
let rec w t =
match t with
| ((SimpleBranch(a,b)), (SimpleBranch(c,d))) ->  b + d
| ((CompoundBranch(c,mo)), (SimpleBranch(a,b))) -> (w mo) + b
| ((SimpleBranch(a,b)), (CompoundBranch(c,mo))) -> b + (w mo)
| ((CompoundBranch(len,mo)), (CompoundBranch(len2,mo2))) -> (w mo)+(w mo2)  in
let rec bal m =
match m with
| ((SimpleBranch(a,b)), (SimpleBranch(c,d))) -> (a*b) = (c*d)
| ((CompoundBranch(c,sum)), (SimpleBranch(a,b))) -> (bal sum) && (c * (w sum)) = (a*b)
| ((SimpleBranch(a,b)), (CompoundBranch(c,sum))) -> (bal sum) && (a*b) = (c * (w sum))
| ((CompoundBranch(c,sum)), (CompoundBranch(d,sum2))) -> (bal sum) && (bal sum2) && (c * (w sum)) = (d * (w sum)) in bal m;;