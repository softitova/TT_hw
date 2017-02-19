let rec merge cmp x y = match (x,y) with 
	| ([],_) -> y
    	| (_,[]) -> x
	| (h1::t1, h2::t2) -> 
          if cmp h2 h1 
	      then h1::(merge cmp t1 y)
          else h2::(merge cmp x t2)

and split x y z = match x with
	| [] -> (y,z)
	| x::other -> split other z (x::y)

and merge_sort cmp x = match x with
        | ([] | _::[]) -> x
	| _ -> let (pri,seg) = split x [] [] 
in (merge cmp (merge_sort cmp pri) (merge_sort cmp seg));;

let l = merge_sort (>) [2;6;1;8];;

let rec print_list l lst =
	match l with 
	|[] -> print_int lst
	|head::tail -> print_list tail (lst*10 + head);;


print_list l 0;;
