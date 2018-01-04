type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun(a, b) ->
match a with
|ZERO -> b
|SUCC m -> natadd (m, SUCC b)

let rec natmul : nat * nat -> nat = fun(a, b) ->
match a with
|ZERO -> ZERO
|SUCC m -> natadd(b, natmul(m, b))
(*
let _ =
    let rec int_of_nat' (n: nat) (accum: int): int =
        match n with
        | ZERO -> accum
        | SUCC n' -> int_of_nat' n' (accum+1)
    in
    let string_of_nat (n: nat): string =
        int_of_nat' n 0 |> string_of_int
    in
    let rec nat_of_int (n: int): nat =
        match n with
        | 0 -> ZERO
        | n -> SUCC (nat_of_int (n-1))
    in
    let assert_equal (expected: nat) (actual: nat) =
        if expected = actual then print_endline "true"
        else
            let expected_str = string_of_nat expected in
            let actual_str = string_of_nat actual in
            Printf.printf "Expected %s but actual %s\n" expected_str actual_str
    in
    let test_natadd (a: int) (b: int) (expected: int) =
        let natA = nat_of_int a in
        let natB = nat_of_int b in
        let natExptected = nat_of_int expected in
        natadd (natA, natB) |> assert_equal natExptected
    in
    let test_natmul (a: int) (b: int) (expected: int) =
        let natA = nat_of_int a in
        let natB = nat_of_int b in
        let natExptected = nat_of_int expected in
        natmul (natA, natB) |> assert_equal natExptected
    in
    test_natadd 1 3 4;
    test_natadd 0 1 1;
    test_natadd 10 0 10;
    test_natadd 0 0 0;
    test_natadd 14 3 17;
    test_natmul 1 3 3;
    test_natmul 0 3 0;
    test_natmul 7 0 0;
    test_natmul 4 9 36;
    test_natmul 0 0 0;
*)
