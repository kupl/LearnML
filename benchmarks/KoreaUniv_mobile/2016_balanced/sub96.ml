exception NotImplemented
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob -> let rec wei x = match x with
  | (SimpleBranch(i, j), SimpleBranch(k, l)) -> j + l
  | (SimpleBranch(i, j), CompoundBranch(k, l)) -> j + wei l
  | (CompoundBranch(i, j), SimpleBranch(k, l)) -> wei j + l
  | (CompoundBranch(i, j), CompoundBranch(k, l)) -> wei j + wei l
  in match mob with
  | (SimpleBranch(a, b), SimpleBranch(c, d)) -> if a <= 0 || b <= 0 || c <= 0 || d <= 0 then raise NotImplemented else a * b = c * d
  | (SimpleBranch(a, b), CompoundBranch(c, d)) -> if a <= 0 || b <= 0 then raise NotImplemented else a * b = c * wei d
  | (CompoundBranch(a, b), SimpleBranch(c, d)) -> if c <= 0 || d <= 0 then raise NotImplemented else a * wei b = c * d
  | (CompoundBranch(a, b), CompoundBranch(c, d)) -> a * wei b = c * wei d
