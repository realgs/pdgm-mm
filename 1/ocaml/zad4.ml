let rec fact(n) =
	if (n < 0) then failwith "n < 0"
	else if (n = 0) then 1
	else fact(n - 1) * n;;
	
fact(5);;
fact(0);;
fact(-1);;