type nat = ZERO
         | SUCC of nat

(* b�� SUCC �����̸� b���� SUCC�� ������ z�� natadd�� �θ���,
   b���� ������ SUCC�� �տ� �߰��Ѵ�. *)
let rec natadd ((a : nat), (b : nat)) =
  match b with
  | ZERO   -> a
  | SUCC z -> (SUCC (natadd (a, z)))

(* 2 x 3 = 2 + ( 2 x 2 ) *)
let rec natmul ((a :nat), (b : nat)) =
  match b with
  | ZERO   -> ZERO
  | SUCC z -> (natadd (a, (natmul (a, z))))
