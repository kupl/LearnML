(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool =
  fun (lb,rb) ->
    let rec calc_w : branch -> int = 
      fun b -> match b with
      | SimpleBranch (l, t) -> t
      | CompoundBranch (l, (lft, rgt)) -> (calc_w lft) + (calc_w rgt)
    in
    match lb with
    | SimpleBranch(ll, lw) ->
      (
      match rb with
      | SimpleBranch(rl, rw) ->
        ll * (calc_w lb) = rl * (calc_w rb)
      | CompoundBranch(rl, rm) ->
        ll * (calc_w lb) = rl * (calc_w rb)
      )
    | CompoundBranch(ll, lm) ->
      (
      match rb with
      | SimpleBranch(rl, rw) ->
        ll * (calc_w lb) = rl * (calc_w rb)
      | CompoundBranch(rl, rm) ->
        ll * (calc_w lb) = rl * (calc_w rb)
      )

