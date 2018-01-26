
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec wint : branch -> int
  = fun bra ->
      match bra with
        | SimpleBranch(a,b) -> b
        | CompoundBranch(a,b) -> 
            match b with
                |(c,d) -> wint c + wint d 

  let tint : branch -> int
  = fun bra ->
      match bra with
        | SimpleBranch(a,b) -> a*b
        | CompoundBranch(a,b) -> a*wint(bra)

  let rec balanced : mobile -> bool
  = fun mob -> (* TODO *)
      match mob with
      | (a,b) ->
          if tint(a) = tint(b) then
              match mob with
              | (SimpleBranch(a,b),SimpleBranch(c,d)) -> true
              | (SimpleBranch(a,b),CompoundBranch(c,d)) -> balanced(d)
              | (CompoundBranch(a,b),SimpleBranch(c,d)) -> balanced(b)
              | (CompoundBranch(a,b),CompoundBranch(c,d)) -> balanced(d) && balanced(b)
          else false