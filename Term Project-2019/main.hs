import ColorMain


printArr :: [Color] -> IO ()
printArr []    = putStrLn "\n"
printArr (x:xs) = do
                  -- Print hsv and rgb values
                  putStr $ (show x ++ " " ++ (show $ hsv2rgb x) ++ " ")
                  -- Print hsv description
                  hsv2desc x
                  printArr xs

main :: IO ()
main = do putStrLn "Enter a starting color name"
          color <- getLine
          let startingColor = name2rgb color
          putStrLn "Enter a ending color name"
          color <- getLine
          let endingColor = name2rgb color
          putStrLn "Enter step number"
          stepNum <- getLine
          let step = read stepNum :: Int
          let myArr = hsvGradient (rgb2hsv startingColor) (rgb2hsv endingColor) step
          printArr myArr
