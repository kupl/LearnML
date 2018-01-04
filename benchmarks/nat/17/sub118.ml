type nat = ZERO
  | SUCC of nat

let rec natsub: nat*nat->nat =
    fun (x,tmp) ->
        if x=ZERO then ZERO
          else if x=(SUCC tmp) then tmp
            else natsub(x, SUCC tmp)

let rec natadd: nat*nat->nat =
    fun (a,b) ->
        if b=ZERO then a
          else natadd(SUCC a, natsub(b,ZERO))

let rec natmul: nat*nat->nat =
    fun (a,b) ->
        if b=ZERO then ZERO
          else natadd(natmul(a,natsub(b,ZERO)),a)

