let rec mult list =
	if (list = []) then failwith "empty list"
	else if (List.tl list = []) then List.hd list
	else List.hd list * mult(List.tl list);;
	
mult [5;7;2];;
mult [];;