type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let diff(exp, var) =
    let rec diff2(exp, var) = match exp with
    | Const(i) -> Const(0)
    | Var(s) -> if s = var then Const(1) else Const(0)
    | Power(s, i) ->
            if s = var
            then Times[Const(i); Power(s, i-1)]
            else Const 0
    | Times(h::t) -> (
        Sum[Times[diff2(h, var); Times(t)];
            Times[h; diff2(Times(t), var)]]
    )
    | Times([]) -> Const 0
    | Sum(h::t) -> Sum [diff2(h, var); diff2(Sum(t), var)]
    | Sum([]) -> Const 0
    in
    let rec reduce exp = match exp with
    | _ -> exp
    in
    reduce (diff2(exp, var))

    (*
;;

diff (Sum[Times[Var "a"; Power("x", 2)]; Times[Var("b"); Var("x")]; Var("c")], "x");;

diff (Const 2, "x");;
diff (Var "y", "y");;
diff (Power("y", 2), "y");;
diff (Power("y", 0), "y");;
diff (Power("y", 1), "y");;
*)
