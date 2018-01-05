(* Exercise 2 *)
exception Error of string

type real = float

let rec sigma (a, b, f) =
        match (a - b) with
                0 ->
                        (f a)
                | _ ->
                        (if (a > b) then
				0.
                        else
				((f a) +. (sigma ((a + 1), b, f))))

let rec pi (a, b, f) =
	match (a - b) with
		0 ->
			(f a)
		| _ ->
			(if (a > b) then
				0.
			else
				((f a) *. (pi ((a + 1), b, f))))

let sumprod (matrix, n, k) =
	if ((n <= 0) || (k <= 0)) then
		raise (Error "Numbers must be greater than zero")
	else
		(let f i j = pi (1, j, (fun x -> matrix (i, x))) in
		sigma (1, n, (fun x -> (f x k))))
