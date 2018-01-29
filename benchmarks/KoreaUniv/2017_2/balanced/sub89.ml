(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> let rec sum mo = match mo
with (l,r) -> match l 
with SimpleBranch (xl, wl) -> ( match r 
with SimpleBranch (xr, wr) -> wl + wr
| CompoundBranch (lr, mr) -> wl + (sum mr))
| CompoundBranch (ll, ml) -> ( match r
with SimpleBranch (xr, wr) -> wr + (sum ml)
| CompoundBranch (lr, mr) -> (sum ml) + (sum mr))

in let rec bal m = match m
with (l,r) -> match l
with SimpleBranch (xl, wl) -> ( match r 
with SimpleBranch (xr, wr) -> xl * wl = xr * wr
| CompoundBranch (lr, mr) -> (xl * wl = lr * (sum mr)) && (bal mr))
| CompoundBranch (ll, ml) -> ( match r
with SimpleBranch (xr, wr) -> (xr * wr = ll * (sum ml)) && (bal ml)
| CompoundBranch (lr, mr) -> (bal ml) && (bal mr) && 
((sum ml)*ll ==  (sum mr)*lr) )

in bal m