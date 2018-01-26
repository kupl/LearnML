let rec prime : int -> bool
= fun n -> 
	let rec isPrime a = if a * a <= n then n > 1 && n mod a <> 0
	&& isPrime (a + 1) else n > 1 in isPrime 2
