
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob -> 
    let rec calW (b1, b2) =
        (match b1 with 
        | SimpleBranch(l, w) -> w
        | CompoundBranch(l, m) -> calW m) +
        (match b2 with 
        | SimpleBranch(l, w) -> w
        | CompoundBranch(l, m) -> calW m)
    in let cal b = 
      match b with 
      | SimpleBranch(l, w) -> l * w 
      | CompoundBranch(l, m) -> l * calW m
    in match mob with
    | (b1, b2) -> cal b1 == cal b2