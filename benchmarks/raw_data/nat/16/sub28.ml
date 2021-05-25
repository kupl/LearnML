type nat =
        | ZERO
        | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun a b ->
let rec count nat =
match nat with
| ZERO ->0
|SUCC(a) -> 1 + count a
in
let rec add num nat =
if num =0 then nat
else add (num-1) (SUCC(nat))
in
let cnt = count(a) in
add cnt b


let rec natmul : nat -> nat -> nat
= fun a b ->
let rec count nat =
match nat with
| ZERO ->0
|SUCC(a) -> 1 + count a
in
let rec add num nat =
if num =0 then nat
else add (num-1) (SUCC(nat))
in
let ca = count(a) in
let cb = count(b) in
add (ca*cb) (ZERO)

