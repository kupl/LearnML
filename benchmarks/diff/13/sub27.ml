(* 2 *)
exception InvalidArgument
type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let rec diff (exp, str) = 
        match exp with
        | CONST c -> (CONST 0)
        | VAR x ->
          if x = str then (CONST 1)
          else (CONST 0)
        | POWER (x, p) ->
          if p = 0 then (CONST 0)
          else if x = str then (TIMES [(CONST p); POWER(x, p-1)])
          else (CONST 0)
        | TIMES l ->
          (match l with
          | [] -> raise InvalidArgument
          | _ -> (SUM (make_times_list [] l str))
          )
        | SUM l -> 
          (match l with
          | [] -> raise InvalidArgument
          | _ -> (SUM (make_sum_list l str))
          )

and make_times_list prefix l x = 
        match l with
        | [] -> []
        | h::t ->
          let d = diff(h, x) in
          if d = (CONST 0) then (make_times_list (h::prefix) t x)
          else TIMES(d::(List.append prefix t))::(make_times_list (h::prefix) t x)

and make_sum_list l x = 
        match l with
        | [] -> []
        | h::t -> diff(h, x)::(make_sum_list t x)