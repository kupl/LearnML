
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec get_PW tree = 
          match tree with
          | SimpleBranch(len, wet) -> wet 
          | CompoundBranch(len, mob) ->
                   match mob with 
                   | (m,n) -> (get_PW m) + (get_PW n);;
  let rec get_W tree = 
          match tree with
          | SimpleBranch(len, wet) -> len*wet 
          | CompoundBranch(len, mob) ->
                    match mob with (m,n) -> 
                    if (get_W n)=(get_W m) then ((get_PW m)+(get_PW n))*len  else (-1);;

  let balanced : mobile -> bool
  = fun mob -> (*raise NotImplemented *)
      match mob with (left_Ch, right_Ch) -> if (get_W left_Ch)=(get_W right_Ch) then true else false;;
