(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool =
  fun m ->
    let rec sum_weight tree =
      match tree with
      (SimpleBranch(e1, e2), SimpleBranch(e3, e4)) -> e2 + e4
      | (SimpleBranch(e1, e2), CompoundBranch(e3, e4)) -> e2 + (sum_weight e4)
      | (CompoundBranch(e1, e2), SimpleBranch(e3, e4)) -> (sum_weight e2) + e4
      | (CompoundBranch(e1, e2), CompoundBranch(e3, e4)) -> (sum_weight e2) + (sum_weight e4)
    in
      (match m with
      (SimpleBranch(e1, e2), SimpleBranch(e3, e4)) ->
        let v1 = (e1 * e2) in let v2 = (e3 * e4) in 
          if v1 = v2 then true else false
      | (SimpleBranch(e1, e2), CompoundBranch(e3, e4)) ->
        if (balanced e4) then let v1 = e1 * e2 in
          let v2 = e3 * (sum_weight e4) in if v1 = v2 then true else false
        else false
      | (CompoundBranch(e1, e2), SimpleBranch(e3, e4)) ->
        if (balanced e2) then let v1 = e1 * (sum_weight e2) in
          let v2 = e3 * e4 in if v1 = v2 then true else false
        else false
      | (CompoundBranch(e1, e2), CompoundBranch(e3, e4)) ->
        if (balanced e2) && (balanced e4) then let v1 = e1 * (sum_weight e2) in
          let v2 = e3 * (sum_weight e4) in if v1 = v2 then true else false
        else false);;