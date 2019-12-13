type aexp =
    | Const of int
    | Var of string
    | Power of string * int
    | Times of aexp list
    | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
    match exp with
    | Const _ -> Const 0
    | Var v -> if v = x then Const 1 else Const 0
    | Power (v, n) ->
        if v = x then
            if n = 0 then Const 0
            else if n = 1 then Const 1
            else if n = 2 then Times [ Const n; Var v ]
            else Times [ Const n; Power (v, n - 1) ]
        else Const 0
    | Times xs ->
        let rec bind s l =
            match s with
            | t :: ts -> bind ts (t :: l)
            | [] -> l in
        let rec times s lst =
            let group lst =
                match lst with
                | [e] -> e
                | _ -> Times lst in
            match lst with
            | e :: es ->
                (match diff (e, x) with
                | Const 0 -> times (e :: s) es
                | Const 1 -> (group (bind s es)) :: times (e :: s) es
                | e' -> (group (bind s (e' :: es))) :: times (e :: s) es)
            | [] -> [] in
        (match times [] xs with
        | [] -> Const 0
        | [e] -> e
        | terms -> Sum terms)
    | Sum xs ->
        let rec sum lst =
            match lst with
            | e :: es ->
                (match diff (e, x) with
                | Const 0 -> sum es
                | e' -> e' :: sum es)
            | [] -> [] in
        (match sum xs with
        | [] -> Const 0
        | [e] -> e
        | terms -> Sum terms)
;;

diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;
