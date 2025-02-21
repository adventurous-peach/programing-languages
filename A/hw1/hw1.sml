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

(*
* Date list * int -> int
* Count how many Dates in a given list happened in m
* number_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 5) = 1
* number_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 6) = 0
* number_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 2) = 2
* number_in_month([], 12) = 0
*)
fun number_in_month(x: (int * int * int) list, m: int) =
  if null x
  then 0
  else if #2 (hd x) = m
       then 1 + number_in_month(tl x, m)
       else number_in_month(tl x, m)

(*
* Date list * int list -> int
* Count how many Dates in a given list happened in any of the m in the given list
* number_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [5, 3]) = 1
* number_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [6, 12, 7]) = 0
* number_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [4, 5]) = 2
* number_in_months([], [12, 4, 5, 2]) = 0
*)
fun number_in_months(x: (int * int * int) list, m: int list) =
  if null m
  then 0
  else number_in_month(x, hd m) + number_in_months(x, tl m)

(* Date list * int -> Date list
* Filter a given list of Dates keeping only the Dates that happened in m
* dates_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 5) = [(12, 5, 23)]
* dates_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 6) = []
* dates_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 2) = [(1, 2, 3), (3, 2, 24)]
* dates_in_month([], 12) = []
*)
fun dates_in_month(x: (int * int * int) list , m: int) =
  if null x
  then []
  else if #2 (hd x) = m
       then hd x :: dates_in_month(tl x, m)
       else dates_in_month(tl x, m)

(* Date list * int list -> Date list
* Filter a given list of Dates keeping only the Dates that happened in any m of a given list
* dates_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [5, 3]) = [(12, 5, 23)]
* dates_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [6, 12, 7]) = []
* dates_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [4, 5]) = [(3, 4, 30), (12, 5, 23)]
* dates_in_months([], [12, 4, 5, 2]) = []
*)
fun dates_in_months(x: (int * int * int) list, m: int list) =
  if null m
  then []
  else dates_in_month(x, hd m) @ dates_in_months(x, tl m)

(* string list * int -> string
* Return the nth element of a given list of strings (1-indexed)
* get_nth(["a", "b", "c", "d", "e"], 5) = "e"
* get_nth(["a", "b", "c", "d", "e"], 3) = "c"
* get_nth(["a", "b", "c", "d", "e"], 1) = "a"
*)
fun get_nth(x: string list, n: int) =
  if n = 1
  then hd x
  else get_nth(tl x, n - 1)

(*
* Date -> string
* Cast a given Date into a string
* date_to_string(2000, 6, 8) = "June 8, 2000"
* date_to_string(1996, 8, 1) = "August 1, 1996"
* date_to_string(2025, 2, 24) = "February 24, 2025"
*)
fun date_to_string(d: int * int * int) =
  let 
    val m = ["January", "February", "March", "April", "May", "June", "July",
             "August", "September", "October", "November", "December"]
  in get_nth(m, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d) end

(*
* int * int list -> int
* Count how many of the first elements of a given list of int add up to less than sum
* number_before_reaching_sum(4, [4, 3, 5]) = 0
* number_before_reaching_sum(7, [4, 3, 5]) = 1
* number_before_reaching_sum(8, [4, 3, 5]) = 2
* number_before_reaching_sum(8, [1, 1, 1]) = raise NoMoreElements
*)
fun number_before_reaching_sum(sum: int, x: int list) =
  let
    exception NoMoreElements
    fun helper_fun(y: int list, acc: int, elem: int) =
      if null y
      then raise NoMoreElements
      else let
             val acc = acc + hd y
           in 
             if acc >= sum
             then elem
             else helper_fun(tl y, acc, elem + 1) end 
  in helper_fun(x, 0, 0) end

(*
* int -> int
* Return what month a given day (out of the 365) is in
* what_month(1) = 1
* what_month(31) = 1
* what_month(50) = 2
* what_month(365) = 12
*)
fun what_month(d: int) =
  let 
    val m = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum(d, m) + 1 end

(* int * int -> int list
* ???
* month_range(1, 3) = [1, 1, 1]
* month_range(31, 31) = [1]
* month_range(31, 32) = [1, 2]
* month_range(31, 30) = []
*)
fun month_range(d1: int, d2: int) =
  if d1 > d2
  then []
  else what_month(d1) :: month_range(d1 + 1, d2)
