type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

let remove_nth l n =
  let rec remove l n c = 
    if c = n then List.tl l
    else (List.hd l)::(remove (List.tl l) n (c+1))
  in
  (remove l n 0)

let rec diff (ae,str) = 
  let diff_times(l,v) =
    let rec diff_times'(l,v,cur) = 
      if cur > (List.length l) - 1 then []
      else TIMES(diff(List.nth l cur,v)::(remove_nth l
      cur))::(diff_times'(l,v,cur+1))
    in
    SUM(diff_times'(l,v,0))
  in
  let diff_sum(l,v) = 
    let rec ds(l,v) = 
      match l with
      | [] -> []
      | hd::tl -> diff(hd,v)::ds(tl,v)
    in
    SUM(ds(l,v))
  in
  match ae with
  | CONST i -> CONST 0
  | VAR s -> if s = str then CONST 1 else CONST 0
  | POWER (s,i) -> 
      if s = str 
      then TIMES(CONST i::[POWER (s, i-1)])
      else CONST 0
  | TIMES l -> diff_times(l,str)
  | SUM l -> diff_sum(l,str)
