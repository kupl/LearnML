(* problem 3*)
  type formula =
  True
  |False
  |Var of string
  |Neg of formula
  |And of formula*formula
  |Or of formula*formula
  |Imply of formula*formula
  |Iff of formula*formula

  let sat: formula -> bool
  = fun f ->
    let rec eval input =
     match input with
      |True -> True
      |False -> False
      |Var str ->
        if str = "P" then True
        else False
      |Neg a ->
        if eval a = True then False
        else True
      |And (a1, a2) ->
        if eval a1 = True && eval  a2 = True then True
        else False
      |Or (b1, b2) ->
        if eval b1=True || eval b2=True then True
        else False
      |Imply (c1, c2) ->
        if eval c1=False then True
        else if eval c1=True || eval c2=True then True
        else False
      |Iff (d1, d2) ->
        if eval d1 = eval d2 then True
        else False in
      if eval f = True then true
      else false;;
       
