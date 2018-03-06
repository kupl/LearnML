type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec calculate : mobile -> weight
 = fun (lb, rb) ->
  match lb, rb with
  | SimpleBranch(l1,w1), SimpleBranch(l2,w2) -> w1+w2
  | SimpleBranch(l1,w1), CompoundBranch(l2,m) -> 
    ( match m with 
      | l, r -> w1 + calculate(l,r) 
    )
  | CompoundBranch(l1,m), SimpleBranch(l2,w2) -> 
    ( match m with 
      | l, r -> w2 + calculate(l,r) 
    )
  | CompoundBranch(l1,m1), CompoundBranch(l2,m2) -> 
    ( match m1 with 
      | l, r -> 
        (match m2 with
          | ll, rr -> calculate(l,r) + calculate(ll,rr)
        )
    )

let balanced : mobile -> bool
=fun (lb,rb) ->
  match lb, rb with
  | SimpleBranch(l1,w1), SimpleBranch(l2,w2) ->
    if l1*w1 = l2*w2 then true 
    else false
  | SimpleBranch(l1,w1), CompoundBranch(l2,m) ->
    ( match m with 
      | l, r -> if l1*w1 = l2*calculate(l,r) then true
    else false
    )
  | CompoundBranch(l1,m), SimpleBranch(l2,w2) ->
    ( match m with 
      | l, r -> if l2*w2 = l1*calculate(l,r) then true
    else false
    )
  | CompoundBranch(l1,m1), CompoundBranch(l2,m2) ->
    ( match m1 with 
      | l, r ->
        (match m2 with
          | ll, rr -> if l1*calculate(l,r) = l2*calculate(ll,rr) then true
            else false
        )
    )
