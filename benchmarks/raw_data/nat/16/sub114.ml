type nat =
	| ZERO
	| SUCC of nat

let rec natadd x y =
match y with
|ZERO -> x
|SUCC (y1) -> SUCC (natadd x y1)

let rec natmul x y =
match y with
|ZERO -> ZERO
|SUCC (y1) -> natadd x (natmul x y1)
