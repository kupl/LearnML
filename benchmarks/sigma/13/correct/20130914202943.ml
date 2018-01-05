let sigma (a, b, f) =
	let sum = ref 0 in for n = a to b do sum := !sum + f(n) done; !sum
