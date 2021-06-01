let rec iter = (fun (n, f) ->
				if n < 0 then raise Exit
				else if n =0 then (fun x -> x)
				else (fun x -> (iter (n -1, f)) (f x)));;
