(*
어려우니 여기부터 기록해라. Done

nat을 인자로 받는 SUCC
type shape = Rect of int * int | Circle of int;;
Rect (1, 2)
*)
type nat = ZERO | SUCC of nat

(*
SUCC가 위의 shape에서 Circle처럼 인자를 받으니, 
인자로 구분하면 되네
*)
let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
    match n1 with
      | SUCC (w) -> SUCC (natadd w n2)
      | ZERO -> n2;;

(*
n1에 있는 nat개수만큼 계속 n2를 더해주기.
2 * 3 = 3 + 3에 착안
*)
let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
      | SUCC (w) -> natadd n2 (natmul w n2)
      | ZERO -> ZERO;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
let four = SUCC (SUCC (SUCC (SUCC ZERO)));;
let oh = ZERO;;

natadd oh two;;
natadd two oh;;

natmul oh two;;
natmul two oh;;


natmul two three;;    (*SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))*)
natmul three two;;    (*SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))*)
natmul three three;;  (*SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))))))*)
natmul four three;;   (*SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))))))))*)
natmul three four;;   (*SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))))))))*)

natadd two four;;   (*SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))))))))*)
natadd three three;;   (*SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))))))))*)
natadd four four;;   (*SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))))))))*)
natadd three two;;   (*SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))))))))*)


