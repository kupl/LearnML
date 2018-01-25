(*problem 1*)
  type btree = Empty|Node of int*btree*btree
  let rec mirror: btree -> btree
  = fun t ->
    match t with
    Empty -> Empty
    |Node(n, left, right) ->
    if left=Empty && right=Empty then Empty
    else
      Node(n, mirror right, mirror left);;

(*problem 2*)
  type nat = ZERO|SUCC of nat
  let natadd: nat -> nat -> nat
  = fun n1 n2 ->
    let add =
      let rec toInt input =
        match input with
        ZERO -> 0
        |SUCC n -> 1 + (toInt n) in
      toInt n1 + toInt n2 in
    let rec toNat outcome =
      match outcome with
      0 -> ZERO
      |1 -> SUCC ZERO
      |_ -> SUCC (toNat(outcome-1)) in
    toNat add;;

  let natmul: nat -> nat -> nat
  = fun n1 n2 ->
    let mul =
      let rec toInt input =
        match input with
        ZERO -> 0
        |SUCC n -> 1 + (toInt n) in
      toInt n1 * toInt n2 in
    let rec toNat outcome =
      match outcome with
      0 -> ZERO
      |1 -> SUCC ZERO
      |_ -> SUCC (toNat(outcome-1)) in
    toNat mul;;

  let rec natexp: nat -> nat -> nat
  = fun n1 n2 ->    
    let expv =  
       let rec toInt input =
       match input with
       ZERO -> 0
       |SUCC n -> 1 + (toInt n) in
       let rec exp a b =
       if b = 0 then 0
       else a * (exp a (b-1)) in
    exp (toInt n1) (toInt n2) in
      let rec toNat outcome =
        match outcome with
        0 -> ZERO
        |1 -> SUCC ZERO
        |_ -> SUCC (toNat(outcome-1)) in
    toNat expv;;

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
       
