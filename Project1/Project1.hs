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
    


sundays1Recursive :: Integer -> Integer -> Integer
sundays1Recursive start end = sundaysRec' start 1 0
  where
    sundaysRec' :: Integer -> Integer -> Integer -> Integer
	sundaysRec' y m current
	  | y > end = current
	  | otherwise = if dayOfWeek y m 1 == 1 then sundaysRec' nextY nextM current+1 else  sundaysRec' nextY nextM current
	    where
		  nextY = if m == 12 then y + 1 else y
		  nextM = if m == 12 then 1 else m + 1



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



sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays2' 2 start 0 1	-- Start  with weekday 2 and n value 0 from start year and month 1
  where
    sundays2' :: Integer -> Integer -> Integer -> Integer -> Integer
	sundays2' weekday y n m
	  | y > end = n 		-- If recursion ends return total n 
	  | m > 12 =  sundays2' nextWeekDay (y+1) n 1  	-- Start recursion for next year with month 
	  | otherwise = sundays2' nextWeekDay y nextN nextM  -- Recursive loop for year y which increase only month  
	    where 
	  	  days :: Integer
	  	  days = daysInMonth m y

	  	  nextWeekDay :: Integer
	  	  nextWeekDay = weekday + mod days 7 

	  	  nextM :: Integer
	  	  nextM =  m + 1   	-- Increase month after month 12 the if statement will set it to 1 so it will be max 13 and recursion will finish
					
	  	  nextN :: Integer
	  	  nextN = if mod nextWeekDay 7 == 0 then n + 1 else n  -- If it's Sunday increase counter

		
	daysInMonth :: Integer -> Integer -> Integer
	daysInMonth month year
      | month == 2  = if leap year then 29 else 28
	  | month == 4 || month == 6 || month == 9 || month == 11 = 30
	  | otherwise = 31
	    where
	      leap :: Integer -> Bool
	      leap year
	        | mod year 4 == 0 && mod year 100 /= 0 || mod year 400 == 0 = True
	    	| otherwise = False