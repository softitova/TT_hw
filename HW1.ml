
type peano = Z|S of peano;;

let rec add x y = match y with
	Z-> x
	|S yy -> S(add x yy);;

let rec sub x y = match (x, y) with 
	(x, Z)-> x
	|(Z, y) -> Z
	|(S x, S y)-> sub x y;;

let rec mul x y = match (x, y)  with
	(x, Z) -> Z
	|(Z, y) -> Z
	|(x, S y) -> add (mul x y) (x);;

let rec power x y = match (x, y) with
	(x, Z) -> S Z
	|(Z, x) -> Z
	|(S Z, x) -> S Z
	|(x, S y) -> mul (power x y) (x);;

let peano_of_int n =
	 let rec make_from_int n p_n =
	 	 match n with
		 	0 ->p_n
			| x -> S(make_from_int (x - 1) p_n) in
	  make_from_int n Z;;

let int_of_peano p_n =
	let rec make_from_peano p_n str =
		match p_n with
			Z -> "0"^str
			|S x -> make_from_peano x "'"^str in
	make_from_peano p_n "";;

let inc p_n = add(p_n)(S Z);;

let rec div x y = match sub (inc (x)) (y)  with
        Z -> Z
        |_ -> add (S Z) (div  (sub x y) (y));;


print_string(int_of_peano (div (peano_of_int(2)) (peano_of_int (1)))^"\n");;
print_string(int_of_peano (div (peano_of_int(8)) (peano_of_int (3)))^"\n");;
print_string(int_of_peano (div (peano_of_int(2)) (peano_of_int (5)))^"\n");;
print_string(int_of_peano (div (peano_of_int(0)) (peano_of_int (1)))^"\n");;

let dec p_n = 
 	match p_n with
		Z -> Z
		|_ -> sub(p_n)(S Z);;

print_string("powerree testing\n");;

print_string(int_of_peano (power (peano_of_int 2) (peano_of_int 2))^" ");;
print_string(int_of_peano (power (peano_of_int 0) (peano_of_int 2))^" ");;
print_string(int_of_peano (power (peano_of_int 1) (peano_of_int 2))^" ");;
print_string(int_of_peano (power (peano_of_int 1) (peano_of_int 0))^" ");;
print_string(int_of_peano (power (peano_of_int 3) (peano_of_int 0))^"\n");;

print_string("substract testing\n");; 

print_string(int_of_peano (sub (peano_of_int 0) (peano_of_int 2))^" ");;
print_string(int_of_peano (sub (peano_of_int 1) (peano_of_int 2))^" ");;
print_string(int_of_peano (sub (peano_of_int 1) (peano_of_int 0))^" ");;
print_string(int_of_peano (sub (peano_of_int 3) (peano_of_int 1))^"\n");;

print_string("add testing\n");;

print_string(int_of_peano (add (peano_of_int 0) (peano_of_int 2))^" ");;
print_string(int_of_peano (add (peano_of_int 1) (peano_of_int 2))^" ");;
print_string(int_of_peano (add (peano_of_int 1) (peano_of_int 0))^" ");;
print_string(int_of_peano (add (peano_of_int 3) (peano_of_int 1))^"\n");;

print_string("multiply testing\n");;

print_string(int_of_peano (mul (peano_of_int 0) (peano_of_int 2))^" ");;
print_string(int_of_peano (mul (peano_of_int 1) (peano_of_int 2))^" ");;
print_string(int_of_peano (mul (peano_of_int 1) (peano_of_int 0))^" ");;
print_string(int_of_peano (mul (peano_of_int 3) (peano_of_int 2))^"\n");;

print_string("increment testing\n");;

print_string(int_of_peano (inc (peano_of_int 2))^" ");;
print_string(int_of_peano (inc (peano_of_int 0))^"\n");;

print_string("decrement testing\n");;

print_string(int_of_peano (dec (peano_of_int 2))^" ");;
print_string(int_of_peano (dec (peano_of_int 0))^" ");;
