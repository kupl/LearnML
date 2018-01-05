type nat = ZERO
  |SUCC of nat
let rec natadd (left,right) = 
  match (left,right) with
  |(SUCC(a),right) -> natadd (a,SUCC(right))
  |(ZERO,right) -> right
let rec natmul (left,right) =
  match (left,right) with
  |(ZERO,right) -> ZERO
  |(SUCC(ZERO),right) -> right
  |(SUCC(a),right) -> natadd (right,natmul (a,right))

(*
natadd:nat*nat->nat
natmul:nat*nat->nat
natadd(ZERO,(SUCC (SUCC ZERO)))=(SUCC (SUCC ZERO))
*)
