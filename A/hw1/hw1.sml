
fun is_older(d1: int * int * int, d2: int * int* int) =
  if #1 d1 <> #1 d2
  then #1 d1 < #1 d2
  else if #2 d1 <> #2 d2
       then #2 d1 < #2 d2
       else #3 d1 < #3 d2

fun number_in_month(x: (int * int * int) list, m: int) =
  if null x
  then 0
  else if #2 (hd x) = m
       then 1 + number_in_month(tl x, m)
       else number_in_month(tl x, m)

fun number_in_months(x: (int * int * int) list, m: int list) =
  if null m
  then 0
  else number_in_month(x, hd m) + number_in_months(x, tl m)

fun dates_in_month(x: (int * int * int) list , m: int) =
  if null x
  then []
  else if #2 (hd x) = m
       then hd x :: dates_in_month(tl x, m)
       else dates_in_month(tl x, m)

fun dates_in_months(x: (int * int * int) list, m: int list) =
  if null m
  then []
  else dates_in_month(x, hd m) @ dates_in_months(x, tl m)

fun get_nth(x: string list, n: int) =
  if n = 1
  then hd x
  else get_nth(tl x, n - 1)

fun date_to_string(d: int * int * int) =
  let 
    val m = ["January", "February", "March", "April", "May", "June", "July",
             "August", "September", "October", "November", "December"]
  in get_nth(m, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d) end
  
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

fun what_month(d: int) =
  number_before_reaching_sum(d, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

fun month_range(d1: int, d2: int) =
  if d1 > d2
  then []
  else what_month(d1) :: month_range(d1 + 1, d2)

fun oldest(x: (int * int * int) list) =
  if null x
  then NONE
  else let
         fun helper_fun(y: (int * int * int) list, acc: (int * int * int)) =
           if null y
           then acc
           else if is_older(hd y, acc)
                then helper_fun(tl y, hd y)
                else helper_fun(tl y, acc)
       in SOME(helper_fun(tl x, hd x)) end
