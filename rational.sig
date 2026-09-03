signature RATIONAL =
  sig
    type rational

    exception RationalException
    
    val new: IntInf.int * IntInf.int -> rational
    val rat: IntInf.int -> rational
    val ratinv: IntInf.int -> rational
    val  == : rational * rational -> bool
    val  << : rational * rational -> bool
    val  ++ : rational * rational -> rational
    val  ~~ : rational -> rational
    val inverse : rational -> rational
    val  **  : rational * rational -> rational
    val  // : rational * rational -> rational
    val  -- : rational * rational -> rational
    val num : rational -> IntInf.int
    val den : rational -> IntInf.int
    val show: rational -> string
  end
