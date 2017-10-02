
module NextNumber where


main = do
  cs <- gets
  let spls = drop 2 $ reverse $ zip (inits cs) (tails cs) of
     pure $ case find (\(_, a:b:_) -> a < b) spls of
        Nothing         -> b:'0':as ++ bs
            where (as, b:bs) = span (== '0') $ reverse cs
        Just (xs, y:ys) -> xs ++ b:as ++ y:bs
            where (as, b:bs) = span (<= y)   $ reverse ys

