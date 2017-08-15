module MaxSubarray where

kadane :: [Integer] -> (Integer, Int, Int)
kadane xs = kadaneHelper xs 0 0 0 0 0 0
  where kadaneHelper [] maxSum _ _ fStart fEnd _ = (maxSum, fStart, fEnd)
        kadaneHelper (y:ys) maxSum maxStart maxEnd fStart fEnd currSum =
          case (currSum + y > maxSum) of
            True -> kadaneHelper ys (currSum + y) maxStart (maxEnd + 1) maxStart (maxEnd + 1) (currSum + y)
            _ -> case (currSum + y < 0) of
                   True -> kadaneHelper ys maxSum (maxEnd + 1) (maxEnd + 1) fStart fEnd 0
                   _    -> kadaneHelper ys maxSum maxStart maxEnd fStart fEnd (currSum + y)
