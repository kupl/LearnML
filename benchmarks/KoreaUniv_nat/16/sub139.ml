type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> let total =
let rec length nt =
match nt with
|ZERO -> 0
|SUCC ZERO -> 1
|SUCC(x) -> 1+(length x) in
(length n1)+(length n2) in
let rec makenat t =
match t with
|0 -> ZERO
|1 -> SUCC ZERO
|_ -> SUCC (makenat (t-1)) in
makenat total;;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let total =
let rec length nt =
match nt with
|ZERO -> 0
|SUCC ZERO -> 1
|SUCC(x) -> 1+(length x) in
(length n1)*(length n2) in
let rec makenat t =
match t with
|0 -> ZERO
|1 -> SUCC ZERO
|_ -> SUCC (makenat (t-1)) in
makenat total;;
