type nat = ZERO | SUCC of nat
let rec natadd1 : nat * nat * nat -> nat = fun(a,b,c) ->
  if b<=c then a
  else natadd1(SUCC a, b, SUCC c)
let natadd : nat * nat -> nat = fun (a,b) ->
  natadd1(a, b, ZERO)

let rec natmul1 : nat * nat * nat * nat * nat -> nat = fun(a,b,c,d,e) ->
  if c<=e then a
  else if b<=d then natmul1(a, b, c, ZERO, SUCC e)
  else natmul1(SUCC a, b, c, SUCC d, e)

let natmul : nat * nat -> nat = fun (a,b) ->
  if a<=ZERO then ZERO
  else if b<=ZERO then ZERO
  else natmul1(a,a,b,ZERO,SUCC ZERO)
