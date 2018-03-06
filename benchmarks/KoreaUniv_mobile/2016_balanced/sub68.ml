
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
  let rec g br = match br with SimpleBranch (l, w) -> w | CompoundBranch (l, m) -> (match m with (a, b) -> (g a) + (g b));;
  let rec f br = match br with SimpleBranch (l, w) -> l*w | CompoundBranch (l, m) -> (match m with (a, b) -> if (f a)=(f b) then (((g a)+(g b))*l) else (-1));;
  let balanced : mobile -> bool
  = fun (lb, rb) -> if ((f lb) = (f rb)) then true else false;;
