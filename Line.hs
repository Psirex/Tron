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
  let p = intersect (fromPoint p1 p2) (fromPoint p3 p4)
      x1 = min (fst p1) (fst p2)
      y1 = min (snd p1) (snd p2)
      x2 = max (fst p1) (fst p2)
      y2 = max (snd p1) (snd p2)
      x3 = min (fst p3) (fst p4)
      y3 = min (snd p3) (snd p4)
      x4 = max (fst p3) (fst p4)
      y4 = max (snd p3) (snd p4) in
    case p of
      Nothing -> False
      Just(x, y) -> if x >= x1 && y >= y1 && x <= x2 && y <= y2 &&
        x >= x3 && y >= y3 && x <= x4 && y <= y4 then True
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
