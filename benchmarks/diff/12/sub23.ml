type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

let rec diff(e, v) = 
  match e with
  |CONST(a) -> CONST(0)
  |VAR(a) -> 
      if a = v then
        CONST(1)
      else
        CONST(0)
  |POWER(a, n) ->
      if a = v then
        if n = 0 then 
          CONST(0)
        else
          TIMES[CONST(n);POWER(a, n-1)]
      else
        CONST(0)
  |TIMES(l) -> (
    match l with 
    |[] -> CONST(1)
    |h::t -> 
        if diff(h,v) = CONST(0) then
          TIMES([h;diff(TIMES(t),v)])
        else if diff(h,v) = CONST(1) then
          SUM([ TIMES(t) ; TIMES([h;diff(TIMES(t),v)]) ])
        else
          SUM([ TIMES([diff(h,v)]@t) ;TIMES([h;diff(TIMES(t),v)]) ])
  )
  |SUM(l) -> (
    match l with
    |[] -> CONST(0)
    |h::t -> 
        if diff(h,v) = CONST(0) then
          diff(SUM(t),v)
        else
          SUM([diff(h,v) ; diff(SUM(t),v)])
  )
