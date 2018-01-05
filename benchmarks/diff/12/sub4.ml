type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

let rec diff (x,y) =
        match x with
                CONST c -> CONST 0
                |VAR a -> if (String.compare a y) = 0 then CONST 1
                                else CONST 0
                |POWER(a,b) -> if String.compare a y != 0 then CONST 0
                                else if b = 1 then (VAR a)
                                else if b = 2 then TIMES[CONST 2;VAR a]
                                        else TIMES[CONST b;POWER(a,b-1)]
                |SUM l -> (match l with
                                [] -> CONST 0
                                |(h::t) -> SUM[diff(h,y);diff(SUM t,y)])
                |TIMES l -> (match l with
                                [] -> CONST 0
                                |(h::[]) -> diff(h,y)
                                |((CONST a)::t) -> TIMES[CONST a;diff(TIMES t,y)]
                                |(h::t) -> SUM[TIMES[diff(h,y);TIMES t];TIMES[h;diff(TIMES t,y)]])