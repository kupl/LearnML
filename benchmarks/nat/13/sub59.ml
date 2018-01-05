type nat=
    ZERO
	|SUCC of nat

let rec natadd : nat*nat->nat=
    fun (x,y)->
	    match x with
		|ZERO -> y
		|SUCC n -> (natadd (n,(SUCC y)))

let rec natmul : nat*nat->nat=
    fun (x,y)->
	    match x with
		|ZERO -> ZERO
		|SUCC n -> (natadd (y,(natmul (n,y))))
