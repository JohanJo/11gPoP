module Graph
type point = (float * float)
type polyline = point list

/// Add two points
let addPoints p0 p1 : point =
  (fst p0 + fst p1, snd p0 + snd p1)

/// Translate point array
let translatePoints d l : polyline =
  List.map (addPoints d) l

/// Scale a point
let scalePoint s p : point =
  (s * fst p, s * snd p)

/// Scale point array
let scalePoints s l : polyline =
  List.map (scalePoint s) l

/// Rotate a point
let rotatePoint theta p : point =
  ((fst p) * cos theta - (snd p) * sin theta, (fst p) * sin theta + (snd p) * cos theta)

/// Rotate point array
let rotatePoints theta l : polyline =
  List.map (rotatePoint theta) l

/// Calculate center of mass
let mean l : point =
  let sum = List.reduce addPoints l
  scalePoint (1.0 / (float l.Length)) sum

/// Find the maximum value of each coordinate element in a list
let maximum (c : polyline) =
  let maxPoint p1 p2 =
    (max (fst p1) (fst p2), max (snd p1) (snd p2))
  List.fold maxPoint (-infinity, -infinity) c

/// Find the minimum value of each coordinate element in a list
let minimum (c : polyline) =
  let minPoint p1 p2 =
    (min (fst p1) (fst p2), min (snd p1) (snd p2))
  List.fold minPoint (infinity, infinity) c
