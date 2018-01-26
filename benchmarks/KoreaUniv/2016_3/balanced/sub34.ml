
  type mobile = branch * branch
  and branch =
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec plus : branch -> int
  = fun p ->
  match p with
  | SimpleBranch(m, n) -> n
  | CompoundBranch(m, (n, o)) -> (plus n) + (plus o);;

  let rec mul : branch -> int
  = fun mo ->
  match mo with
  | SimpleBranch(a, b) -> a * (plus mo)
  | CompoundBranch(a, (b, c)) -> a * (plus mo);;

  let rec balanced : mobile -> bool
  = fun mob ->
  let rec bal : branch -> bool
  = fun br ->
  match br with
  | SimpleBranch(l, w) -> true
  | CompoundBranch(l, (w1, w2)) -> if (mul w1) = (mul w2) then (bal w1) && (bal w2)
  else false
  in
  match mob with
  | (a, b) -> ((bal a) && (bal b)) && ((mul a) = (mul b))