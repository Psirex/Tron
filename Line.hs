module Line where

data Line = Line {
  a :: Float,
  b :: Float,
  c :: Float
} deriving(Show)

det :: Float -> Float -> Float -> Float -> Float
det a b c d = a * d - b * c

fromPoint ::(Float, Float) -> (Float, Float) -> Line
fromPoint (x1, y1) (x2, y2) = Line {a = a',b = b',c = c'}
  where
    a' = y1 - y2
    b' = x2 - x1
    c' = x1 * y2 - x2 * y1

intersectLocal :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
intersectLocal p1 p2 p3 p4 =
  let p = intersect (fromPoint p1 p2) (fromPoint p3 p4) in
    case p of
      Nothing -> False
      Just(x, y) -> if x >= fst p1 && y >= snd p1 && x <= fst p2 && y <= snd p2 &&
        x >= fst p3 && y >= snd p3 && x <= fst p4 && y <= snd p4 then True
        else False

equivalent :: Line -> Line -> Bool
equivalent (Line a1 b1 c1) (Line a2 b2 c2) =
  abs(det a1 b1 a2 b2) < 0.00001 && abs(det a1 c1 a2 c2) < 0.00001 &&
  abs(det b1 c1 b2 c2) < 0.00001

intersect :: Line -> Line -> Maybe(Float, Float)
intersect (Line a1 b1 c1) (Line a2 b2 c2) =
  let z = det a1 b1 a2 b2 in
  if abs z < 0.00001 then
    Nothing
  else Just (-det c1 b1 c2 b2 / z, -det a1 c1 a2 c2 / z)
  -- where
  --   z = det a1 b1 a2 b2
  --   if (abs z) < 0.00001 then
  --     res = Nothing
  --   else
  --     res = (-det c1 b1 c2 b2 / z, -det a1 c1 a2 c2 / z)
