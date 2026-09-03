structure rational :> RATIONAL = struct
    type rational = { num : IntInf.int, den : IntInf.int }

    exception RationalException

    local
      fun gcd (0, b) : IntInf.int = b
        | gcd (a, b) = gcd (b mod a, a)
    in
      fun norm (p : rational) : rational =
            let val a = #num p and b = #den p
                val g = gcd (IntInf.abs a, IntInf.abs b)
            in
              if b = 0 then raise RationalException
              else  let val c = a * b
                    in if c = 0 then {num = 0, den = 1}
                       else if c > 0
                            then { num = IntInf.abs a div g, den = IntInf.abs b div g }
                            else { num = ~(IntInf.abs a div g), den = IntInf.abs b div g }
                    end
            end
    end

    fun new (a : IntInf.int, b : IntInf.int) : rational =
       if b = 0 then raise RationalException
       else (norm { num = a, den = b })

    fun rat (a : IntInf.int) : rational = new (a, 1)

    fun ratinv ( a : IntInf.int) : rational = if a = 0 then raise RationalException
                        else new (1, a)

    infix <<
    fun (p : rational) << (q : rational) =
	  let val np : rational = norm p and nq : rational = norm q
		val      nnp = #num np and dnp = #den np
		    and  nnq = #num nq and dnq = #den nq
		val left = nnp * dnq and right = dnp * nnq
         in left < right
	   end

    infix ==
    fun (p : rational) == (q : rational) = (norm p = norm q)

    infix ++
    fun (p : rational) ++ (q : rational) =
          let val a = #num p
              and b = #den p
              and c = #num q
              and d = #den q
              val r = {num = a*d + b*c, den = b*d}
          in
            norm r
          end

    fun ~~(p : rational) : rational = {num = 0 - (#num p), den = #den p}

    infix --
    fun (p : rational) -- (q : rational) = p ++ (~~ q)

    infix **
    fun  (p : rational) ** (q : rational) =
          let val a = #num p
              and b = #den p
              and c = #num q
              and d = #den q
              val r : rational = {num = a*c, den = b*d}
          in
            norm r
          end

    fun inverse (p : rational) : rational = norm (new(#den p, #num p))

    infix //
    fun (p : rational) // (q : rational) = p ** (inverse q)

    fun num (r : rational) = #num r
    fun den (r : rational) = #den r

    fun show (p : rational) =
                    let val q : rational = norm p
                        val a = #num q
                        and b = #den q
                        val c = IntInf.toString a
                        val d = IntInf.toString b
                    in
                        if (a = 0) orelse (b = 1)
                        then c ^ ""
                        else c ^ "/" ^ d ^ ""
                    end
  end
