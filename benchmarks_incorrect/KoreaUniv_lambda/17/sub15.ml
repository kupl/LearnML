(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
let rec recheck 
=fun v lamm->match lamm with

|P(x,l)->recheck (v@[x]) l
|C(l1,l2)->recheck v l1 && recheck v l2
|V x ->(match v with 
|[]->false
|hd::tl-> if hd = x then true else recheck tl lamm)

let rec check : lambda -> bool
=fun lam -> match lam with
|P(v,l)-> recheck [v] l
|_->false
