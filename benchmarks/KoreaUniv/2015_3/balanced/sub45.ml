  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
let balanced : mobile -> bool
=fun (lb,rb) ->
  let rec mw : mobile -> int
  = fun (llb , rrb) -> (bw llb ) + (bw rrb)
  and bw : branch -> int
  = fun bb ->
     match bb with
     | SimpleBranch ( l , w) -> w
     | CompoundBranch (l , m) -> mw m in
  let balance_sub_sub : int -> int -> int -> int -> bool
  = fun ll lw rl rw -> (ll * lw) = (rl * rw) in
  let rec balanced_sub : mobile -> bool
  = fun (lb , rb) ->
    match lb , rb with
    | SimpleBranch (ll , lw) , SimpleBranch (rl , rw) -> balance_sub_sub ll lw rl rw
    | SimpleBranch (ll , lw) , CompoundBranch (rl , rm) -> (balance_sub_sub ll lw rl (mw rm)) && (balanced_sub rm)
    | CompoundBranch (ll , lm) , SimpleBranch (rl , rw) -> (balance_sub_sub ll (mw lm)  rl rw) && (balanced_sub lm)
    | CompoundBranch (ll , lm) , CompoundBranch (rl , rm) -> ((balance_sub_sub ll (mw lm) rl (mw rm)) && (balanced_sub lm)) && (balanced_sub rm) in
  balanced_sub (lb , rb)

