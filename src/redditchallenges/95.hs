module Challenge95 where

sgen = rnd 123456789 where rnd n = n:rnd ((22695477 * n + 12345) `mod` 1073741824)

rectangles p s = rects sgen where
    rects (a:b:c:d:ss) = (x1,y1,x2,y2):rects ss where
        x1 = a`mod`p
        y1 = b`mod`p
        x2 = x1 + 1 + (c`mod`s)
        y2 = y1 + 1 + (d`mod`s)

area (a,b,c,d) = (c-a) * (d-b)

bbox (x:xs) = bbx x xs where
  bbx a [] = a
  bbx (a,b,c,d) ((a1,b1,c1,d1):xs) = bbx (min a a1, min b b1, max c c1, max d d1) xs

clip _ [] = []
clip bb ((x1,y1,x2,y2):rs)
    | a1 == a2 || b1 == b2 = []
    | a1 >= x2 || a2 <= x1 || y1 >= b2 || y2 <= b1 = clip bb rs
    | True = (max a1 x1, max b1 y1, min a2 x2, min b2 y2) : clip bb rs
    where (a1,b1,a2,b2) = bb

cover bb [] = 0
cover bb (rc:rs) = area rc + sum [ cover x (clip x rs) | x <- [t,b,l,r] ]
    where
        (a1,b1,a2,b2) = bb
        (x1,y1,x2,y2) = rc
        t = (a1,y2,a2,b2)
        b = (a1,b1,a2,y1)
        l = (a1,y1,x1,y2)
        r = (x2,y1,a2,y2)

main :: IO ()
main = let rs0 = take 100 (rectangles 500 99)
           rs1 = take 2000  (rectangles  2000000 99999)
           rs2 = take 50000 (rectangles 10000000 99999) in
         do print $ cover (bbox rs0) rs0
            print $ cover (bbox rs1) rs1
            print $ cover (bbox rs2) rs2
