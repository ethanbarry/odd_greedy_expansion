use "rational.sig";
use "rational.sml";

exception DenominatorNotOdd
exception ImproperFraction
exception UnreducedInputs

(* Given an implementation of rational numbers with arbitrary precision
 * from the included headers, we can implement the Odd Greedy Expansion
 * algorithm recursively.
 *)
fun oge (0, _) = print "0\n" 
  | oge (1, b) = print ("1/" ^ IntInf.toString b ^ "\n")
  | oge (a, b) =
    if b mod 2 = 0 then raise DenominatorNotOdd
    else if a > b then raise ImproperFraction
    else
    (* STEPS:
     * 1. u = min{ odds greater than y div x }
     *    i.e. if floor(y/x) is even then floor(y/x)+1 else floor(y/x)+2
     * 2. include 1/u in the expansion and recurse on x/y - 1/u.
     * 3. terminate when the remainder is a unit fraction
     *    you haven't already included.
     *)
        let
          open rational (* Bring my arbitrary-precision rational
                           datatype into scope *)
          infix == --   (* Allow these functions to be infix operators *)
          val firstr = new (a, b) (* Construct a rational *)
          fun recurse (r : rational, acc) =
            (* If we're given a unit fraction with num = 1,
             * & the denominator is not equal to the previous
             * one, then it must be unique and it is the final
             * term in the sum. We're done; reverse and return the list.
             *)
            if num r = 1 andalso 
               (den (hd acc) handle _ => IntInf.fromInt 1) <> den r 
               then List.rev (r :: acc)
            else
              let
                (* The floor of the inverse, or the nearest
                   integer below this rational *)
                val flr = IntInf.div (den r, num r)
                (* The nearest odd number above this rational *)
                val u = if flr mod 2 = 0 then flr + 1 else flr + 2
                (* Inverse of the nearest odd integer which we
                   haven't used yet *)
                val uinv = if null acc then
                                ratinv u
                           else if u <= den (hd acc) then
                                ratinv (den (hd acc) + 2)
                           else ratinv u
                (* Difference of r and uinv, or p/q - 1/u. *)
                val remaining = r -- uinv

                val intgrstr = rational.show uinv
              in
                print ("\nSize of denominator: " ^ Int.toString (size intgrstr));
                print ("\nTerm: " ^ intgrstr);
                (* Stick uinv into the sum and split up the remaining amount *)
                recurse (remaining, uinv :: acc)
              end

          (* Look ma, higher-order functions! *)
          fun printlst R = 
            print (String.concatWith ", " (map rational.show R) ^ "\n")
        in
          (* Start with a/b and an empty list *)
          printlst (recurse (firstr, []))
        end

(* Given an implementation of rational numbers with arbitrary precision
 * from the included headers, we can implement the Odd Greedy Expansion
 * algorithm recursively.
 *)
fun oge' (0, _) = rational.rat 0 :: nil 
  | oge' (1, b) = rational.ratinv b :: nil
  | oge' (a, b) =
    if b mod 2 = 0 then raise DenominatorNotOdd
    else if a > b then raise ImproperFraction
    else
    (* STEPS:
     * 1. u = min{ odds greater than y div x }
     *    i.e. if floor(y/x) is even then floor(y/x)+1 else floor(y/x)+2
     * 2. include 1/u in the expansion and recurse on x/y - 1/u.
     * 3. terminate when the remainder is a unit fraction you haven't already included.
     *)
        let
          open rational (* Bring my arbitrary-precision rational datatype into scope *)
          infix == --   (* Allow these functions to be infix operators *)
          val firstr = new (a, b) (* Construct a rational *)
          fun recurse (r : rational, acc) =
            (* If we are given a unit fraction with num = 1, and the denominator is
             * not equal to the previous one, then it must be unique and it is the
             * final term in the sum. We're done; reverse and return the list.
             *)
            if num r = 1 andalso (den (hd acc) handle _ => IntInf.fromInt 1) <> den r then List.rev (r :: acc)
            else
              let
                (* The floor of the inverse, or the nearest integer below this rational *)
                val flr = IntInf.div (den r, num r)
                (* The nearest odd number above this rational *)
                val u = if flr mod 2 = 0 then flr + 1 else flr + 2
                (* Inverse of the nearest odd integer which we haven't used yet *)
                val uinv = if null acc then
                                                   ratinv u
                                               else if u <= den (hd acc) then
                                                   ratinv (den (hd acc) + 2)
                                               else ratinv u
                (* Difference of r and uinv, or p/q - 1/u. *)
                val remaining = r -- uinv
              in
                (* Stick uinv into the sum and split up the remaining amount *)
                recurse (remaining, uinv :: acc)
              end
        in
          (* Start with a/b and an empty list *)
          recurse (firstr, [])
        end

fun testOverRange (p : int, q : int) =
    let
        val p = IntInf.fromInt p
        val q = IntInf.fromInt q
        (* Smallest odd denominator strictly greater than p *)
        val min_q = if IntInf.mod (p, 2) = 0 then p + 1 else p + 2
        
        (* How many odd denominators fit between min_q and max q *)
        val count = IntInf.toInt (if q <= min_q then 0 else ((q - min_q) div 2) + 1)
        
        val qs = List.rev (List.tabulate (count, fn n => (min_q + IntInf.fromInt (2 * n))))
        val ps = List.tabulate (count, fn _ => p)

        val pqs = ListPair.zip (ps, qs)
        
        fun process (a, b) =
            let
                val orig = IntInf.toString a ^ "/" ^ IntInf.toString b ^ " = "
                val expansion = oge' (a, b)
                val line = orig ^ String.concatWith ", " (List.map rational.show expansion) ^ "\n"
            in
                print line
            end
    in
        List.app process pqs
    end
