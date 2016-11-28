val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
'6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ" )

mnem flatMap { case (k, s) => s map { (_, k) } }

for ( (d, s) <- mnem; c <- s) yield c -> d