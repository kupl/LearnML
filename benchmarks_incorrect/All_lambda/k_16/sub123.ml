
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec makePList lambda pl = match lambda with
  | V v -> pl
  | P (v, e) -> pl @ [v] @ makePList e pl
  | C (e1, e2) -> makePList e1 pl @ makePList e2 pl

  let rec makeVList lambda vl = match lambda with
  | V v -> vl @ [v]
  | P (v, e) -> vl @ makeVList e vl
  | C (e1, e2) -> makeVList e1 vl @ makeVList e2 vl

  let rec compareToPlist pl e = match pl with
  | [] -> false
  | hd :: tl -> (e = hd) || (compareToPlist tl e)

  let rec compareList pl vl = match vl with
  | [] -> true
  | hd :: tl -> (compareList pl tl) && (compareToPlist pl hd)



  let rec check : lambda -> bool
  = fun lambda -> match lambda with
  | V v -> false
  | P (v, e)->
      let pl = makePList lambda []
        in let vl = makeVList lambda []
          in compareList pl vl
  | C (e1, e2) -> check e1 && check e2
