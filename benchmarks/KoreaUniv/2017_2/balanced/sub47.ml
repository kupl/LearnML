(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec mobile_weight m =
        match m with
        | (SimpleBranch (_, lw), SimpleBranch (_, rw)) -> lw + rw
        | (SimpleBranch (_, lw), CompoundBranch (_, rm)) -> lw + (mobile_weight rm)
        | (CompoundBranch (_, lm), SimpleBranch (_, rw)) -> (mobile_weight lm) + rw
        | (CompoundBranch (_, lm), CompoundBranch (_, rm)) -> (mobile_weight lm) + (mobile_weight rm)

let rec balanced : mobile -> bool
= fun m ->
        match m with
        | (SimpleBranch (ll, lw), SimpleBranch (rl, rw)) ->
                if ll * lw = rl * rw then true else false
        | (SimpleBranch (ll, lw), CompoundBranch (rl, rm)) ->
                if balanced rm then
                        if ll * lw = rl * (mobile_weight rm) then true else false
                else false
        | (CompoundBranch (ll, lm), SimpleBranch (rl, rw)) ->
                if balanced lm then
                        if ll * (mobile_weight lm) = rl * rw then true else false
                else false
        | (CompoundBranch (ll, lm), CompoundBranch (rl, rm)) ->
                match (balanced lm, balanced rm) with
                | (true, true) -> if ll * (mobile_weight lm) = rl * (mobile_weight rm) then true else false
                | _ -> false
