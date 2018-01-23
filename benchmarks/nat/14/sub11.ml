type nat = ZERO
         | SUCC of nat

(* b가 SUCC 형태이면 b에서 SUCC를 제거한 z로 natadd를 부르고,
   b에서 제거한 SUCC를 앞에 추가한다. *)
let rec natadd ((a : nat), (b : nat)) =
  match b with
  | ZERO   -> a
  | SUCC z -> (SUCC (natadd (a, z)))

(* 2 x 3 = 2 + ( 2 x 2 ) *)
let rec natmul ((a :nat), (b : nat)) =
  match b with
  | ZERO   -> ZERO
  | SUCC z -> (natadd (a, (natmul (a, z))))
