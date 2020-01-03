(* 1 *)
let rec multiply xs =
    if(xs = []) then failwith "an empty list"
    else if(List.tl xs != []) then List.hd xs * multiply(List.tl xs)
    else List.hd xs ;;

(* 2 *)
let rec concatenate (xs, terminator, separator) =
    if(xs = []) then ""
    else if(List.tl xs != []) then List.hd xs ^ separator ^ concatenate(List.tl xs, terminator, separator)
    else List.hd xs ^ terminator ;;

(* 3 *)
let rec check_sign xs =
    if(xs = []) then true
    else if(List.hd xs < 0) then false
    else check_sign(List.tl xs) ;;

(* 4 *)
let rec factorial x =
    if (x < 0) then failwith "less than zero"
    else if (x = 0 || x = 1) then 1
    else x * factorial(x - 1) ;;
