
  type exp =
      | V of var
      | P of var * exp
      | C of exp * exp
  and var = string
  let rec check : exp -> bool
    = fun exp -> 

      (*let lst = [](*default list*)*)



      let rec finalcheck e l =(*check variable form by list, return bool*)
        match l with
          |[] -> false
          |hd::tl -> if hd == e then true else (finalcheck e tl)


      in let rec checklst e l = (*make variable list, check form by procedure list, return bool *)
        match e with
          |P(v,e)-> (checklst e (l@[v]))
          |C(e1,e2) -> (checklst e1 l)&&(checklst e2 l)

          |V(v) -> (finalcheck v l)


      (*match l with
        |[] -> false
        |hd::tl -> if hd == v then true else (checklst e tl)*)

      in (checklst exp [])
