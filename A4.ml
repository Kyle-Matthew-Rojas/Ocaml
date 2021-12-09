(* CSC 345-01 Assignment #4
   On my honor, Kyle Matthew Rojas, this assignment is my own work.  
   I, Kyle Matthew Rojas, will follow the instructor's rules and processes 
   related to academic integrity as directed in the course syllabus.

   *** You are NOT allowed to use OCaml's pattern matching for this assignment. ***

   *** DO NOT CHANGE ANYTHING IN THIS FILE EXCEPT THE PLACEHOLDERS. ***
*) 



(* 1. Write a function averageThree to return the average of three integers. 
      For example, averageThree (-1) 1 2;; returns 0.66666666666666663 *)
let averageThree : int -> int -> int -> float = fun x y z -> (float_of_int(x) +. float_of_int(y) +. float_of_int(z)) /. float_of_int(3);;



(* 2. Write a function howManyAboveAverage that returns how many of three integer inputs are above its average value.  
      For example, howManyAboveAverage 1 3 3;; returns 2 *)
let howManyAboveAverage : int -> int -> int -> int = fun x y z -> let avg = (x + y + z) / 3 in if (x > avg && y > avg && z > avg) then 3 else if (x < avg && y < avg && z < avg) then 0 else if (x = avg && y = avg && z = avg) then 0 else if (x > avg && y < avg && z < avg) || (x < avg && y > avg && z < avg) || (x < avg && y < avg && z > avg) then 1 else 2;;



(* 3. Write a function howManyWithinThreshold that returns how many of the first three arguments are within the threshold (the fourth argument) of the average of the first three arguments. 
       For example, howManyWithinThreshold 10 1 2 3.5;; returns 2 *)
let howManyWithinThreshold : int -> int -> int -> float -> int = fun x y z t -> let a = float_of_int(x) in let b = float_of_int(y) in let c = float_of_int(z) in if ( a < t && b < t && c< t) then 3 else if (a < t && b < t && c > t) then 2 else if (a > t && b < t && c < t) then 2 else if (a < t && c < t && b > t) then 2 else if ( a < t && b > t && c > t) then 1 else if (a > t && c > t && b < t) then 1 else if (c < t && b > t && a > t) then 1 else 0;;



(* 4. Write a function threeDifferent that returns true if no two of the three arguments are equal, and false otherwise.  
      For example, threeDifferent 1 2 2;; returns false *)	  
let threeDifferent : int -> int -> int -> bool = fun x y z -> if (x = y) || (x = z) || (y = x) || (y = z) || (z = x) || (z = y) then false else true;;



(* 5. Write a function sum that uses recursion to compute the sum of all numbers from 1 to n, where n is greater than or equal to 1.
      For example, sum 3;; returns 6 *)
let rec sum : int -> int = fun n -> if n <= 0 then 0 else if n = 1 then n else n + sum(n-1);;



(* 6. Write a function getBinary that uses recursion to convert an integer n (where n is greater than or equal to 0) to its binary representation.
      For example, getBinary 12;; returns 1100 
                   getBinary 7;;  returns 111
                   getBinary 42;; returns 101010 
      Hint:    if n's binary representation is 10010101011
            (n / 2)'s binary representation is 1001010101
            (N % 2)'s binary representation is           1 *)	  
let rec getBinary : int -> int = fun n -> if n > 0 then getBinary (n/2) * 10 + (n mod 2) else 0;;



(* 7. Write a function howManyDigits that uses recursion to count the digits of an integer n (where n is greater than or equal to 1).
      For example, howManyDigits 978;; returns 3 *)
let rec howManyDigits : int -> int = fun n -> if n > 0 then howManyDigits (n / 10) + 1 else 0;;



(* 8. Write a function orderTriple that takes a triple, and returns a version in increasing order.
      For example, orderTriple (2, 1, 3);; returns (1, 2, 3) *)
let orderTriple : int * int * int -> int * int * int = fun (x, y, z) ->  if z < y && y < x  then (z,y,x) else if y < x && x < z then (y,x,z) else if z < x && x < y then (z,x,y) else if y < z && z < x then (y,z,x) else if x < z && z < y then (x,z,y) else (x,y,z);;
