let rec sigma f num1 num2 =
	if (num1 = num2) then (f num1)
	else if (num1 < num2) then (f num1) + (sigma f (num1+1) num2)
	else 0

