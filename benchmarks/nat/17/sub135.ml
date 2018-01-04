type nat = ZERO | SUCC of nat

let rec natadd:nat*nat->nat=fun(a,b)->
  match a with
  |ZERO->b
  |SUCC c->(
    SUCC (natadd(c,b))
  )

let rec natmul:nat*nat->nat=fun(a,b)->
  match a with
  |ZERO->ZERO
  |SUCC c->(
    natadd(b,natmul(c,b))
  )
