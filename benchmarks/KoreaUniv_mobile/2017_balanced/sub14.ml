(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> (* TODO *)
  let rec calcweight comb =
    match comb with
    |SimpleBranch (a1,a2), SimpleBranch (b1,b2) -> a1*a2 + b1*b2
    |SimpleBranch (a1,a2), CompoundBranch (b1,b2) -> a1*a2 + b1*(calcweight b2)
    |CompoundBranch (a1,a2), SimpleBranch (b1,b2) -> a1*(calcweight a2) + b1*b2
    |CompoundBranch (a1,a2), CompoundBranch (b1,b2) -> a1*(calcweight a2) + b1*(calcweight b2) in
      let rec helper m =
        match m with
        |SimpleBranch (a1,a2), SimpleBranch (b1,b2) -> if a1*a2 = b1*b2 then true else false
        |SimpleBranch (a1,a2), CompoundBranch (b1,b2) -> if a1*a2 = b1*(calcweight b2) then true else false
        |CompoundBranch (a1,a2), SimpleBranch (b1,b2) -> if a1*(calcweight a2) = b1*b2 then true else false
        |CompoundBranch (a1,a2), CompoundBranch (b1,b2) -> if a1*(calcweight a2) = b1*(calcweight b2) then true else false in
          helper m
