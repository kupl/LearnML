
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec getmeasure branc =
match branc with
| SimpleBranch (l,w) -> [w]
| CompoundBranch(l,m) ->
 begin match m with
 | (branch0, branch1) -> (getmeasure branch0) @ (getmeasure branch1)
 end

let sum l = List.fold_right (fun x y -> x+y) l 0

let rec torque branc =
match branc with
| SimpleBranch (l,w) -> l* sum(getmeasure branc)
| CompoundBranch (l,m) -> l* sum(getmeasure branc)

let rec balanced m = 
match m with
| (branch0, branch1) -> 
 begin match branch0 with
 | SimpleBranch (l0,w0) -> 
  begin match branch1 with
  | SimpleBranch (l1,w1) -> if (torque branch0) <> (torque branch1) then false else true
  | CompoundBranch (l1, m1) -> if (balanced m1) && (torque branch0) == (torque branch1) then true else false
  end
 |CompoundBranch (l0, m0) ->
  begin match branch1 with
  | SimpleBranch (l1,w1) -> if (balanced m0) && (torque branch0) == (torque branch1) then true else false
  | CompoundBranch (l1, m1) -> if (balanced m0) && (balanced m1) && (torque branch0) == (torque branch1) then true else false
  end
 end
