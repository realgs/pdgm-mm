let rec join (list, sep, endc) =
	if (list = []) then endc
	else if (List.tl list = []) then List.hd(list) ^ join(List.tl(list), sep, endc)
	else List.hd(list) ^ sep ^ join(List.tl(list), sep, endc);;
	
join (["jeden"; "dwa"; "trzy"], ", ", "?");;
join ([], ", ", "?");;