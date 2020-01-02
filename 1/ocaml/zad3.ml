let rec ifPos (list) =
	if (list = []) then true
	else if (List.hd list < 0) then false
	else ifPos(List.tl list);;

ifPos([1;2;3;4]);;
ifPos([1;2;3;4;-5]);;