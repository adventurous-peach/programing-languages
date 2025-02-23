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
* get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Jo") = ["Ce", "Ga", "Fa"]
* get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Po") = []
*)
fun get_substitutions1(x, s) =
   case x of
        [] => []
      | x'::xs => case all_except_option(s, x') of
                   NONE => get_substitutions1(xs, s)
               | SOME l => l @ get_substitutions1(xs, s)

(* string list list * string -> string list
* ???
* get_substitutions2([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Jo") = ["Ga", "Fa", "Ce"]
* get_substitutions2([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Po") = []
*)
fun get_substitutions2(x, s) =
 let
    fun helper_fun(x, acc) =
       case x of
            [] => acc
          | x'::xs => case all_except_option(s, x') of
                           NONE => helper_fun(xs, acc)
                         | SOME y => helper_fun(xs, y @ acc)
 in helper_fun(x, []) end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*
* card -> color
* Return the given card's color
* card_color(Spades, Queen) = Black
* card_color(Clubs, Ace) = Black
* card_color(Diamonds, King) = Red
* card_color(Hearts, Num 10) = Red
*)
fun card_color(c) =
   case c of
        (Spades, _) => Black 
      | (Clubs, _) => Black
      | _ => Red

(*
* card -> rank
* Return the given card's value
* card_value(Spades, Queen) = 10
* card_value(Clubs, Ace) = 11
* card_value(Hearts, Jack) = 10
* card_value(Diamonds, King) = 10
* card_value(Spades, Num 10) = 10
* card_value(Clubs, Num 2) = 2
* card_value(Clubs, Num 5) = 5
*)
fun card_value(c) =
   case c of
        (_, Num n) => n
      | (_, Ace) => 11
      | _ => 10

(* 
* card list -> bool
* all_same_color([(Spades, Ace), (Clubs, King)]) = true
* all_same_color([(Hearts, Ace), (Clubs, King)]) = false
* all_same_color([(Spades, Ace), (Diamonds, King)]) = false
* all_same_color([(Hearts, Ace), (Diamonds, King)]) = true
* all_same_color([]) = true
* all_same_color([(Hearts, Num 2)]) = true
*)
fun all_same_color(cs) =
   case cs of
        [] => true
      | x::[] => true
      | x::y::z => card_color(x) = card_color(y) andalso all_same_color(y::z)
