sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
  where
    sundays' :: Integer -> Integer -> Integer
    sundays' y m
      | y > end = rest
      | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
        where
          nextY = if m == 12 then y + 1 else y
          nextM = if m == 12 then 1 else m + 1 
          rest = if y > end then 0 else sundays' nextY nextM
    dayOfWeek :: Integer -> Integer -> Integer -> Integer
    dayOfWeek year month day  = mod ( day + t + k + floor(fromIntegral k / 4) + floor(fromIntegral j / 4) + (5*j) ) 7 
      -- Helper function takes year month and day returns day as integer.
      -- Starts with Saturday = 1 and continue until Friday = 7         
      where 
          month' 
           | month <= 2 = month + 12
           | otherwise = month

          t :: Integer
          t = floor (fromIntegral (13 * (month' + 1)) / 5.0)
  
          k :: Integer
          k  = mod year 100
  
          j :: Integer
          j  = div year 100 

       

  

