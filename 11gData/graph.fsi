module Graph

type point = float * float
type polyline = point list

val addPoints : p0:point -> p1:point -> point
val translatePoints : d:point -> l:polyline -> polyline
val scalePoint : s:float -> p:point -> point
val scalePoints : s:float -> l:polyline -> polyline
val rotatePoint : theta:float -> p:point -> point
val rotatePoints : theta:float -> l:polyline -> polyline
val mean : l:polyline -> point
val maximum : l:polyline -> point
val minimum : l:polyline -> point
