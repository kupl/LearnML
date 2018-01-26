
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec getWeight : mobile -> int
  = fun mob ->
  let (left, right) = mob in
  match left with
  | SimpleBranch (left_len, left_w) ->
    (match right with
      | SimpleBranch (right_len, right_w) ->
        left_w + right_w
      | CompoundBranch (right_len, right_mob) ->
        left_w + (getWeight right_mob)
    )
  | CompoundBranch (left_len, left_mob) ->
    (match right with
      | SimpleBranch (right_len, right_w) ->
        (getWeight left_mob) + right_w
      | CompoundBranch (right_len, right_mob) ->
        (getWeight left_mob) + (getWeight right_mob)
    )

  let rec balanced : mobile -> bool
  = fun mob ->
  let (left, right) = mob in
  match left with
  | SimpleBranch (left_len, left_w) ->
    (match right with
      | SimpleBranch (right_len, right_w) ->
        left_len * left_w = right_len * right_w
      | CompoundBranch (right_len, right_mob) ->
        (left_len * left_w = right_len * (getWeight right_mob))
        && (balanced right_mob)
    )
  | CompoundBranch (left_len, left_mob) ->
    (match right with
      | SimpleBranch (right_len, right_w) ->
        (left_len * (getWeight left_mob) = right_len * right_w)
        && (balanced left_mob)
      | CompoundBranch (right_len, right_mob) ->
        (left_len * (getWeight left_mob) = right_len * (getWeight right_mob))
        && (balanced left_mob)
        && (balanced right_mob)
    )