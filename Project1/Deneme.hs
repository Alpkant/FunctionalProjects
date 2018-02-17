-- Function takes year month and day returns day as integer.
-- Starts with Saturday = 1 and continue until Friday = 7       
dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek year month day  = mod ( day + t + k + floor(fromIntegral k / 4) + floor(fromIntegral j / 4) + (5*j) ) 7 
    where 
        month' 
         | month <= 2 = month + 12

        t :: Integer
        t = floor (fromIntegral (13 * (month' + 1)) / 5.0)

        k :: Integer
        k  = mod year 100

        j :: Integer
        j  = div year 100
  

