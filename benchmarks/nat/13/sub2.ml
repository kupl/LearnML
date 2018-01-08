type nat = ZERO
| SUCC of nat

let rec natadd ((a : nat), (b : nat)) : nat =
    (
        match a with
            ZERO -> b
        |   SUCC (c : nat) -> natadd(c, (SUCC b))
    )

let natmul ((a : nat), (b : nat)) : nat =
    (
        let rec temp_natmul (a : nat) (b : nat) (c : nat) : nat =
            (
                match (a, b) with
                    (ZERO, _) -> ZERO
                |   (_, ZERO) -> ZERO
                |   ((SUCC ZERO), _) -> natadd(b, c)
                |   (_, (SUCC ZERO)) -> a
                |   ((SUCC (d : nat)), _) ->
                        let (x : nat) = natadd (b, c) in
                        (temp_natmul d b x)
            ) in
        (temp_natmul a b ZERO)
    )