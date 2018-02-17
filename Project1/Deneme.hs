-- Function takes year month and day returns day as integer.
-- Starts with Saturday = 0 and continue until Friday = 6       
dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek year month day  = mod ( day + t + k + floor(fromIntegral k / 4) + floor(fromIntegral j / 4) + (5*j) ) 7 
    where 
        month' 
         | month == 1 = 13
         | month == 2 = 14 

        year' 
         | month == 1 || month == 2 = year - 1

        t :: Integer
        t = floor (fromIntegral (13 * (month' + 1)) / 5.0)

        k :: Integer
        k  = mod year' 100

        j :: Integer
        j  = div year' 100
  
