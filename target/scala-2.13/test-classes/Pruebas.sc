import ConjuntosF.{conjuntoUnitario, pertenece, union, intersect, dif}

val s1 = conjuntoUnitario ( 1 )
val s2 = conjuntoUnitario ( 2 )
val s3 = conjuntoUnitario ( 3 )
val s4 = union ( s1 , s2 )
val s5 = union ( s1 , s3 )
val s6 = union ( s2 , s3 )
val s7 = intersect ( s4 , s5 )
val s8 = intersect ( s4 , s6 )
val s9 = intersect ( s5 , s6 )

pertenece ( 1 , s1 )
assert (pertenece ( 1 , s1 ))