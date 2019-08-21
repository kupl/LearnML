type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff (x,y) =
        match x with
                Const c -> Const 0
                |Var a -> if a=y then Const 1
                                else Const 0
                |Power(a,b) -> if a!=y then Const 0
                                else if b = 1 then (Var a)
                                else if b = 2 then Times[Const 2;Var a]
                                        else Times[Const b;Power(a,b-1)]
                |Sum l -> (match l with
                                [] -> Const 0
                                |(h::t) -> Sum[diff(h,y);diff(Sum t,y)])
                |Times l -> (match l with
                                [] -> Const 0
                                |(h::[]) -> diff(h,y)
                                |((Const a)::t) -> Times[Const a;diff(Times t,y)]
                                |(h::t) -> Sum[Times[diff(h,y);Times t];Times[h;diff(Times t,y)]])
