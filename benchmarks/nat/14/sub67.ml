type nat = ZERO | SUCC of nat

let rec natadd (n,m)=
    let rec to_int x =
        match x with
        | ZERO -> 0
        | SUCC y -> 1 + to_int y in
    (to_int n) + (to_int m)

let rec natmul (n,m)=
    let rec to_int x =
        match x with
        | ZERO -> 0
        | SUCC y -> 1 + to_int y in
    (to_int n) * (to_int m)
