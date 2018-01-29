(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec helpbalanced : mobile -> length*weight*length*weight*bool
= fun m ->
  let (lb,rb) = m in
  match (lb,rb) with
  |(SimpleBranch (ll,lw),SimpleBranch (rl,rw)) ->
    if ll*lw = rl*rw then (ll,lw,rl,rw,true)
    else  (ll,lw,rl,rw,false)
  |(SimpleBranch (ll,lw),CompoundBranch (rl,newm)) ->
    let (sonll,sonlw,sonrl,sonrw,torf) = helpbalanced newm in
    if ll*lw = rl*(sonlw+sonrw) then (ll,lw,rl,(sonlw+sonrw),torf)
    else (ll,lw,rl,(sonlw+sonrw),false)
  |(CompoundBranch (ll,newm),SimpleBranch (rl,rw)) ->
    let (sonll,sonlw,sonrl,sonrw,torf) = helpbalanced newm in
    if ll*(sonlw+sonrw) = rl*rw then (ll,(sonlw+sonrw),rl,rw,torf)
    else (ll,(sonlw+sonrw),rl,rw,false)
  |(CompoundBranch (ll,newm1),CompoundBranch (rl,newm2)) ->
    let (sonll1,sonlw1,sonrl1,sonrw1,torf1) = helpbalanced newm1 in
    let (sonll2,sonlw2,sonrl2,sonrw2,torf2) = helpbalanced newm2 in
    let torf = torf1 && torf2 in
    if ll*(sonlw1+sonrw1) = rl*(sonlw2+sonrw2) then (ll,(sonlw1+sonrw1),rl,(sonlw2+sonrw2),torf)
    else (ll,(sonlw1+sonrw1),rl,(sonlw2+sonrw2),false)
  
let balanced : mobile -> bool
= fun m -> 
  let (a,b,c,d,ret) = helpbalanced m in
  ret