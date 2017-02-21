type 'a my_list =
    | Cons of ('a * 'a my_list)
    | Nil

let rec rev_append l1 l2 = match l1 with
    | Nil   -> l2
    | Cons(l_head, l_tail)  -> rev_append l_tail (Cons(l_head, l2));;
	 
let rec rev l = rev_append l Nil;;

let c = Cons(6, Cons(1,Nil));;

let rec print_list l str =
	match l with
	|Nil -> print_int str
	|Cons(x, y) -> print_list y  (str*10 + x) ;;




print_list c 0;;
let d = rev c;;
print_list d 0;;

let rec merge cmp x y = match (x, y) with 
	|(Nil, _) -> y
	|(_, Nil) -> x
	|((Cons(h1, t1)), (Cons(h2, t2))) ->
	if cmp h1 h2
		then Cons(h1, merge cmp t1 y)
	else Cons(h2, merge cmp x t2);;

let rec split x y z = match x with
	| Nil -> (y,z)
	| Cons(x, other) -> split (other) (z) (Cons(x, y));;
		
let rec merge_sort cmp x = match x with
	| (Nil | Cons(_, Nil)) -> x
	| _ -> let (firstPart, secondPart) = split x Nil Nil in
	(merge cmp (merge_sort cmp firstPart) (merge_sort cmp secondPart));;


let m = merge_sort (<) c;;
 print_list m 0;;
