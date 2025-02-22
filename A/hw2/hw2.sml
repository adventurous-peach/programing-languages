(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* string * string list -> string option
* Return a string option removing s from a l if s is present in l
* assume s is listed once at most
* all_except_option("a", ["a", "b", "c", "d"]) = SOME(["d", "c", "b"])
* all_except_option("a", ["a", "b", "c"]) = SOME(["c", "b"])
* all_except_option("a", ["a"]) = SOME([])
* all_except_option("d", ["a", "b", "c"]) = NONE
*)
fun all_except_option(s, l) =
   let 
      fun is_present(x) =
         case x of
              [] => false
            | x'::xs => same_string(x', s) orelse is_present(xs)
      fun helper_fun(x, acc) =
         case x of
              [] => acc
            | x'::xs => helper_fun(xs, if same_string(x', s) then acc else x'::acc)
   in
      if is_present(l) then SOME(helper_fun(l, [])) else NONE end 

(* string list list * string -> string list
* ???
* get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Jo") = ["Ce", "Fa", "Ga"]
* get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Po") = []
*)
fun get_substitutions1(x, s) =
   case x of
        [] => []
      | x'::xs => case all_except_option(s, x') of
                   NONE => get_substitutions1(xs, s)
               | SOME l => l @ get_substitutions1(xs, s)


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
