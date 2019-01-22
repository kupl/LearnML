
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec makePList exp pl = match exp with
  | V v -> pl
  | P (v, e) -> pl @ [v] @ makePList e pl
  | C (e1, e2) -> makePList e1 pl @ makePList e2 pl

  let rec makeVList exp vl = match exp with
  | V v -> vl @ [v]
  | P (v, e) -> vl @ makeVList e vl
  | C (e1, e2) -> makeVList e1 vl @ makeVList e2 vl

  let rec compareToPlist pl e = match pl with
  | [] -> false
  | hd :: tl -> (e = hd) || (compareToPlist tl e)

  let rec compareList pl vl = match vl with
  | [] -> true
  | hd :: tl -> (compareList pl tl) && (compareToPlist pl hd)



  let rec check : exp -> bool
  = fun exp -> match exp with
  | V v -> false
  | P (v, e)->
      let pl = makePList exp []
        in let vl = makeVList exp []
          in compareList pl vl
  | C (e1, e2) -> check e1 && check e2
