open Hw1;;

print_int (int_of_peano (S (S (Z))));;


print_int(int_of_peano (div (peano_of_int(2)) (peano_of_int (1))));;
print_int(int_of_peano (div (peano_of_int(8)) (peano_of_int (3))));;
print_int(int_of_peano (div (peano_of_int(2)) (peano_of_int (5))));;
print_int(int_of_peano (div (peano_of_int(0)) (peano_of_int (1))));;



print_string("power testing\n");;

print_int(int_of_peano (power (peano_of_int 2) (peano_of_int 2)));;
print_int(int_of_peano (power (peano_of_int 0) (peano_of_int 2)));;
print_int(int_of_peano (power (peano_of_int 1) (peano_of_int 2)));;
print_int(int_of_peano (power (peano_of_int 1) (peano_of_int 0)));;
print_int(int_of_peano (power (peano_of_int 3) (peano_of_int 0)));;

print_string("substract testing\n");;

print_int(int_of_peano (sub (peano_of_int 0) (peano_of_int 2)));;
print_int(int_of_peano (sub (peano_of_int 1) (peano_of_int 2)));;
print_int(int_of_peano (sub (peano_of_int 1) (peano_of_int 0)));;
print_int(int_of_peano (sub (peano_of_int 3) (peano_of_int 1)));;

print_string("add testing\n");;

print_int(int_of_peano (add (peano_of_int 0) (peano_of_int 2)));;
print_int(int_of_peano (add (peano_of_int 1) (peano_of_int 2)));;
print_int(int_of_peano (add (peano_of_int 1) (peano_of_int 0)));;
print_int(int_of_peano (add (peano_of_int 3) (peano_of_int 1)));;

print_string("multiply testing\n");;

print_int(int_of_peano (mul (peano_of_int 0) (peano_of_int 2)));;
print_int(int_of_peano (mul (peano_of_int 1) (peano_of_int 2)));;
print_int(int_of_peano (mul (peano_of_int 1) (peano_of_int 0)));;
print_int(int_of_peano (mul (peano_of_int 3) (peano_of_int 2)));;

print_string("increment testing\n");;

print_int(int_of_peano (inc (peano_of_int 2)));;
print_int(int_of_peano (inc (peano_of_int 0)));;

print_string("decrement testing\n");;

print_int(int_of_peano (dec (peano_of_int 2)));;
print_int(int_of_peano (dec (peano_of_int 0)));;

(*
print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x"));;
*)
