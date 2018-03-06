(*problem 6*)

  type mobile =branch * branch
  and branch=SimpleBranch of length *weight
            |CompoundBranch of length * mobile
  and length = int
  and weight = int

    let rec wei:branch ->int
    =fun n ->
      match n with
      |SimpleBranch(a,b)->a*b
      |CompoundBranch(a,(x,y))->a*(wei(x)+wei(y))
  let balanced : mobile -> bool
  =fun m->
    let rec balance :mobile->bool
  = fun m->
    match m with
    |(SimpleBranch (a,b),SimpleBranch(c,d))->
    if a*b=c*d then true else false
    |(SimpleBranch (a,b),CompoundBranch(c,(x,y)))->
    if a*b=c*(wei(x)+wei(y)) then true && balance(x,y) else false
    |(CompoundBranch (a,(x,y)),SimpleBranch(c,d))->
    if a*(wei(x)+wei(y))=c*d then true && balance(x,y) else false
    |(CompoundBranch(a,(x,y)),CompoundBranch(b,(z,w)))->
    if a*(wei(x)+wei(y))=b*(wei(z)+wei(w)) then balance (x,y) && balance(w,z) else false
  in balance(m)