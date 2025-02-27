(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun is_present(lst, e) =
   case lst of
        [] => false
      | x::xs => x = e orelse is_present(xs, e)

(* put your solutions for problem 1 here *)

fun all_except_option(e, lst) =
   let 
      fun helper_fun(x, acc) =
         case x of
              [] => acc
            | x'::xs => if x' = e then acc @ xs else helper_fun(xs, acc @ [x'])
   in
      if is_present(lst, e) then SOME(helper_fun(lst, [])) else NONE end 

fun get_substitutions1(lst, e) =
   case lst of
        [] => []
      | x::xs => case all_except_option(e, x) of
                   NONE => get_substitutions1(xs, e)
               | SOME v => v @ get_substitutions1(xs, e)

fun get_substitutions2(lst, e) =
 let
    fun helper_fun(x, acc) =
       case x of
            [] => acc
          | x'::xs => case all_except_option(e, x') of
                           NONE => helper_fun(xs, acc)
                         | SOME v => helper_fun(xs, acc @ v)
 in helper_fun(lst, []) end

fun similar_names(lst, {first = f, middle = m, last = l}) =
   let
      fun helper_fun(lst', acc) =
         case lst' of
              [] => acc
            | x::xs => helper_fun(xs, acc @ [{first = x, middle = m, last = l}])
   in helper_fun(get_substitutions2(lst, f), [{first = f, middle = m, last = l}]) end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c) =
   case c of
        (Spades, _) => Black 
      | (Clubs, _) => Black
      | _ => Red

fun card_value(c) =
   case c of
        (_, Num n) => n
      | (_, Ace) => 11
      | _ => 10

fun remove_card(cs, c, e) =
   case cs of
        [] => raise e
      | _ => if is_present(cs, c) 
             then case all_except_option(c, cs) of
                       SOME v => v
             else raise e

fun all_same_color(cs) =
   case cs of
       x::y::z => card_color(x) = card_color(y) andalso all_same_color(y::z)
      | _ => true

fun sum_cards(c) =
   let
      fun helper_fun(x, acc) =
         case x of
              [] => acc
            | x'::xs => helper_fun(xs, acc + card_value(x'))
   in helper_fun(c, 0) end

fun score(c, g) =
   let
      val s = sum_cards(c)
      val pre_score = if s > g then (s - g) * 3 else g - s
   in if all_same_color(c) then pre_score div 2 else pre_score end

fun officiate(c, m, g) =
   let
      fun game(cs, hc, mv) =
         case mv of
              [] => score(hc, g)
            | x::xs => case x of
                            Discard c => game(cs, remove_card(hc, c, IllegalMove), xs)
                          | Draw => case cs of
                                         [] => score(hc, g)
                                       | y::ys => let
                                                     val t = y::hc
                                                  in 
                                                    if sum_cards(t) > g
                                                    then score(t, g)
                                                    else game(ys, t, xs)
                                                  end
   in game(c, [], m) end
