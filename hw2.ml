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
