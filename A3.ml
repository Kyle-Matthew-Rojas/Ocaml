(* CSC 345-01 Assignment #3
   On my honor, Kyle Matthew Rojas, this assignment is my own work.  
   I, Kyle Matthew Rojas, will follow the instructor's rules and processes 
   related to academic integrity as directed in the course syllabus.
 
   *** You are NOT allowed to use OCaml's pattern matching for this assignment. ***

   *** DO NOT CHANGE ANYTHING IN THIS FILE EXCEPT THE PLACEHOLDERS. ***
*) 



(* 1. Write a function opp that returns the boolean complement (opposite) of its argument.  
      For example, opp (1 = 2);; returns true *)	  
let opp : bool -> bool = fun x -> if x then false else true;;



(* 2. Write a function dividesEvenlyByFive that returns whether some dividend is evenly divisible by the divisor 5.
      For example, dividesEvenlyByFive 10;; returns true *)
let dividesEvenlyByFive : int -> bool = fun x -> if x mod 5 = 0 then true else false;;



(* 3. Write a function middle that returns the second greatest of three given arguments.
      For example, middle 1 2 3;; returns 2 *)	  
let middle : 'a -> 'a -> 'a -> 'a = fun x y z -> if (x > y && x < z) || (x > z && x < y) then x else if (y > x && y < z) || (y > z && y < x) then y else z;;



(* 4. Write a function nor that computes the NOR gate of two boolean arguments.
      The NOR gate is a combination OR gate followed by an inverter. 
      Its output is "true" if both inputs are "false."  Otherwise, the output is "false." 
      For example, nor (1=2) (2=3);; returns true *)
let nor : bool -> bool -> bool = fun x y -> if x || y then false else true;;



(* 5. Write a function triangleArea that computers the area of a triangle given its base and height. 
      For example, triangleArea 2 3;; returns 3. *)
let triangleArea : int -> int -> float = fun x y -> float_of_int((x * y) / 2);;



(* 6. Write a function ceilingDecimal that calculates the ceiling of a float, but returns it as an int rather than a float. 
      If you are not familiar with the ceiling funtion, read: https://www.mathsisfun.com/sets/function-floor-ceiling.html 
      For example, ceilingDecimal 15.1;; returns 16 *)
let ceilingDecimal : float -> int = fun x -> int_of_float(ceil x);;



(* 7. Write a function letterGrade that returns the equivalent letter grade for a given numerical integer grade below.

		Letter Grade	Numerical Grade x
		A				93 <= x <= 100
		A-				90 <= x < 93
		B+				87 <= x < 90
		B				83 <= x < 87
		B-				80 <= x < 83
		C+				77 <= x < 80
		C				73 <= x < 77
		C-				70 <= x < 73
		D+				67 <= x < 70
		D				63 <= x < 67
		D-				60 <= x < 63
		F				< 60

      For example, letterGrade 91;; returns A- *)
let letterGrade : int -> string = fun x -> if (x >= 93 && x <= 100) then "A" else if (x >= 90 && x < 93) then "A-" else if (x >= 87 && x < 90) then "B+" else if (x >= 83 && x < 87) then "B" else if (x >= 80 && x < 83) then "B-" else if (x >= 77 && x < 80) then "C+" else if (x >= 73 && x < 77) then "C" else if (x >= 70 && x < 73) then "C-" else if (x >= 67 && x < 70) then "D+" else if (x >= 63 && x < 67) then "D" else if (x >= 60 && x < 63) then "D-" else "F";;



(* 8. Write a function findDays that returns the number of days in a month. 
      The function takes in as input an integer representing a month, and outputs the
      number of days.  You can assume that February has 28 days.
      For example, findDays 12;; returns 31
  
      The function returns -1 for any error cases.
      For example, findDays 13;; returns -1 *)
let findDays : int -> int = fun month -> if month = 1 then 31 else if month = 2 then 28 else if month = 3 then 31 else if month = 4 then 30 else if month = 5 then 31 else if month = 6 then 30 else if month = 7 then 31 else if month = 8 then 31 else if month = 9 then 30 else if month = 10 then 31 else if month = 11 then 30 else if month = 12 then 31 else -1;;
