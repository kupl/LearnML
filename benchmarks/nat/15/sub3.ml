type nat = ZERO | SUCC of nat

let rec natadd =
    fun (f, s) ->
        match s with
        | ZERO -> f
        | SUCC ss -> natadd(SUCC f, ss)

let rec natmul =
    fun (f, s) ->
        match s with
        | ZERO -> ZERO
        | SUCC ss -> natadd(natmul(f, ss), f)
