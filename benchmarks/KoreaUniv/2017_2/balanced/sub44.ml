(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->
  let rec wei mob =
    match mob with
    |(left,right) -> (match left with
                      |SimpleBranch (a,b) -> (match right with
                                              |SimpleBranch (c,d) -> b+d
                                              |CompoundBranch (c,d) -> b+(wei d))
                      |CompoundBranch (a,b) -> (match right with
                                                |SimpleBranch (c,d) -> d + (wei b)
                                                |CompoundBranch (c,d) -> (wei d) + (wei b)))
  in let rec balhelp mob =
    match mob with
    |(left,right) -> (match left with
                      |SimpleBranch (a,b) -> (match right with
                                              |SimpleBranch (c,d) -> if a*b = c*d then true else false
                                              |CompoundBranch (c,d) -> if a*b = c*wei d && balhelp d then true else false)
                      |CompoundBranch (a,b) -> (match right with
                                                |SimpleBranch (c,d) -> if c*d = a*wei b && balhelp b then true else false
                                                |CompoundBranch (c,d) -> if c*wei d = a*wei b && balhelp d && balhelp b then true else false))
    in balhelp m;;