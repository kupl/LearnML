  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string  

  let check e = 
  let vars = ref [] in
  let rec chk e1 =
  (match e1 with
  V v -> List.mem v !vars
    | P (v,e1) -> (vars:=!vars@[v] ; chk e1)
    | C (e1,e2) -> (chk e1 && chk e2))in chk e
