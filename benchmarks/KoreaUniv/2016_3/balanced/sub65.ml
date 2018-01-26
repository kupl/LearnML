
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

(*mobile처리*)
let rec calMob : mobile -> int -> int
= fun mob sumLen ->
  
  match mob with
  |(left, right) -> ( calBranch (left) (-1) sumLen ) + ( calBranch (right) (1) sumLen )


(*branch처리*)
and calBranch : branch -> int -> int -> int
= fun bran direction leng->

  match bran with
  |SimpleBranch(len, wei) -> ( leng + (direction*len) ) * wei
  |CompoundBranch(len, mob) -> calMob mob ( leng + (direction*len) )  




(*왼쪽은 마이너스 오른쪽은 플러스로 하여 토크의 합이 0이 되면 ture인데, 이 함수는 토크의 합 체크용*)
let check : mobile -> int
= fun mob ->  (* TODO *)

  calMob mob 0



let balanced : mobile -> bool
= fun mob ->  (* TODO *)

  (* 입력받은 mobile을 매개변수로 하여 calMob호출. 토크의 합이 0이면 ture*)
  if( (calMob mob 0) = 0 ) then true
  else false

