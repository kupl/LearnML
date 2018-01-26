
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec weight branch =
    match branch with
    | SimpleBranch (l,w) -> w
    | CompoundBranch (l,m) ->
      (match m with (a,b) -> weight a + weight b)

  let rec totalweight branch =
    match branch with
    | SimpleBranch (l,w) -> l*w
    | CompoundBranch (l,m) ->
        match m with (a,b) ->
          (
          if (totalweight a) = (totalweight b) then ((weight a) + (weight b))*l
          else -9797
          )


  let balanced : mobile -> bool
  = fun mob -> (* raise NotImplemented (* TODO *) *)
    match mob with (left, right) ->
      if (totalweight left) = (totalweight right) then true else false