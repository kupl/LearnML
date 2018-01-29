(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> let sum =0 in
  let rec accumulate mo = match mo with
  | SimpleBranch (x1,x2), SimpleBranch (y1, y2)-> 
  sum + x2 + y2
  | SimpleBranch (x1,x2), CompoundBranch (y1, mob) -> 
  sum + x2 + accumulate mob
  | CompoundBranch (x1, mob), SimpleBranch (y1, y2) -> 
  sum + accumulate mob + y2
  | CompoundBranch (x1, mob1), CompoundBranch (y1, mob2) -> 
  sum + accumulate mob1 + accumulate mob2

  in let rec measure mob = match mob with
  |SimpleBranch(a, b), SimpleBranch(c, d) ->
  let ltor = a * b in let rtor = c * d in if ltor = rtor then true else false
  |SimpleBranch(a, b), CompoundBranch(c, d) ->
  let ltor = a * b in let sum =0 in let rtor = c * accumulate d in if ltor = rtor then true else false
  |CompoundBranch (a, b), SimpleBranch(c, d) ->
  let sum =0 in let ltor = a * accumulate b in let sum =0 in let rtor = c * d in if ltor = rtor then true else false 
  |CompoundBranch (a, b), CompoundBranch (c,d) ->
  let sum =0 in let ltor = a* accumulate b in let sum =0 in let rtor = accumulate d in if ltor = rtor then true else false
    in match m with
    | SimpleBranch(a1, a2), SimpleBranch(b1, b2) -> if a1 *a2 = b1 * b2 then true else false
    | SimpleBranch(a1, a2), CompoundBranch(b1, m2) -> if measure m = true 
                                                      then balanced m2 
                                                      else false
    | CompoundBranch (a1, m1), SimpleBranch (b1, b2) -> if measure m = true
                                                        then balanced m1
                                                        else false
    | CompoundBranch (a1, m1), CompoundBranch (b1, m2) -> if measure m = true
                                                        then if balanced m1 = true then balanced m2
                                                              else false
                                                      else false