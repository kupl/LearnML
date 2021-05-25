let rec sigma (num1, num2, f) =
	if (num1 = num2) then (f num1)
	else if (num1 < num2) then (f num1) + (sigma ((num1+1), num2, f))
	else 0

