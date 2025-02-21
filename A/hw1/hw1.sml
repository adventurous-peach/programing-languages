(*
* A Date is a tuple of type (int * int * int) whose 1st int represents a year,
* an int greater than 0, its 2nd int represents a month, an int greater than 0
* and less than 13, and its 3rd int represents a day, an int greater than 0 and
* less than 13.
*)

(*
* Date * Date -> bool
* Check if d1 happened before d2
* is_older((1, 2, 3), (1, 2, 4)) = true
* is_older((1, 2, 3), (1, 2, 2)) = false
* is_older((1, 2, 3), (1, 3, 3)) = true
* is_older((1, 4, 3), (1, 3, 3)) = false
* is_older((2, 4, 3), (1, 3, 3)) = false
* is_older((1, 3, 3), (1, 3, 3)) = false
*)
fun is_older(d1: int * int * int, d2: int * int* int) =
  if #1 d1 <> #1 d2
  then #1 d1 < #1 d2
  else if #2 d1 <> #2 d2
       then #2 d1 < #2 d2
       else #3 d1 < #3 d2
