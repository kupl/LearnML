type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

let diff(exp, var) =
    let rec diff2(exp, var) = match exp with
    | CONST(i) -> CONST(0)
    | VAR(s) -> if s = var then CONST(1) else CONST(0)
    | POWER(s, i) ->
            if s = var
            then TIMES[CONST(i); POWER(s, i-1)]
            else CONST 0
    | TIMES(h::t) -> (
        SUM[TIMES[diff2(h, var); TIMES(t)];
            TIMES[h; diff2(TIMES(t), var)]]
    )
    | TIMES([]) -> CONST 0
    | SUM(h::t) -> SUM [diff2(h, var); diff2(SUM(t), var)]
    | SUM([]) -> CONST 0
    in
    let rec reduce exp = match exp with
    | _ -> exp
    in
    reduce (diff2(exp, var))

    (*
;;

diff (SUM[TIMES[VAR "a"; POWER("x", 2)]; TIMES[VAR("b"); VAR("x")]; VAR("c")], "x");;

diff (CONST 2, "x");;
diff (VAR "y", "y");;
diff (POWER("y", 2), "y");;
diff (POWER("y", 0), "y");;
diff (POWER("y", 1), "y");;
*)
