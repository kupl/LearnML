(* problem 6 *)
type mobile = branch * branch
  and branch = SimpleBranch of length * weight
            | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec balanced : mobile -> bool = fun m -> 
  let rec getmobileweight : mobile -> weight = fun mob -> 
    (match mob with
      | (m1, m2) ->
        (match m1 with 
          | SimpleBranch (_, w1) -> w1
          | CompoundBranch (_, subm) -> getmobileweight subm
        ) + 
        (match m2 with
          | SimpleBranch (_, w2) -> w2
          | CompoundBranch (_, subm) -> getmobileweight subm
        )
    )
  in
  match m with
  | (lb, rb) ->
    let balanceleft = 
      match lb with
      | SimpleBranch (len, wgt) -> (len*wgt, true)
      | CompoundBranch (len, subm) -> (len*(getmobileweight subm), balanced subm)
    in
    let balanceright =
      match rb with
      | SimpleBranch (len, wgt) -> (len*wgt, true)
      | CompoundBranch (len, subm) -> (len*(getmobileweight subm), balanced subm)
    in
    match (balanceleft, balanceright) with
      |((lt, lbool), (rt, rbool)) -> if lbool && rbool then lt=rt else false
