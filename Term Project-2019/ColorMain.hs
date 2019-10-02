module ColorMain where

import Data.Char
import Data.Fixed

data Color = RGB Int Int Int | HSV Float Float Float

instance Show Color where
  show (RGB r g b) = "RGB ( " ++ show r ++ ", "++ show g ++ ", " ++ show b ++ " )"
  show (HSV h s v) = "HSV ( " ++ show h ++ ", "++ show s ++ ", " ++ show v ++ " )"

htmlColors :: [ (String, Color) ]
htmlColors  = [ ("AliceBlue", (RGB 240 248 255)), ("AntiqueWhite", (RGB 250 235 215)), ("Aqua", (RGB 0 255 255)), ("Aquamarine", (RGB 127 255 212)),
                  ("Azure", (RGB 240 255 255)), ("Beige", (RGB 245 245 220)), ("Bisque", (RGB 255 228 196)), ("Black", (RGB 0 0 0)),
                  ("BlanchedAlmond", (RGB 255 235 205)), ("Blue", (RGB 0 0 255)), ("BlueViolet", (RGB 138 43 226)), ("Brown", (RGB 165 42 42)),
                  ("BurlyWood", (RGB 222 184 135)), ("CadetBlue", (RGB 95 158 160)), ("Chartreuse", (RGB 127 255 0)), ("Chocolate", (RGB 210 105 30)),
                  ("Coral", (RGB 255 127 80)), ("CornflowerBlue", (RGB 100 149 237)), ("Cornsilk", (RGB 255 248 220)), ("Crimson", (RGB 220 20 60)),
                  ("Cyan", (RGB 0 255 255)), ("DarkBlue", (RGB 0 0 139)), ("DarkCyan", (RGB 0 139 139)), ("DarkGoldenRod", (RGB 184 134 11)),
                  ("DarkGray", (RGB 169 169 169)), ("DarkGrey", (RGB 169 169 169)), ("DarkGreen", (RGB 0 100 0)), ("DarkKhaki", (RGB 189 183 107)),
                  ("DarkMagenta", (RGB 139 0 139)), ("DarkOliveGreen", (RGB 85 107 47)), ("DarkOrange", (RGB 255 140 0)), ("DarkOrchid", (RGB 153 50 204)),
                  ("DarkRed", (RGB 139 0 0)), ("DarkSalmon", (RGB 233 150 122)), ("DarkSeaGreen", (RGB 143 188 143)), ("DarkSlateBlue", (RGB 72 61 139)),
                  ("DarkSlateGray", (RGB 47 79 79)), ("DarkSlateGrey", (RGB 47 79 79)), ("DarkTurquoise", (RGB 0 206 209)), ("DarkViolet", (RGB 148 0 211)),
                  ("DeepPink", (RGB 255 20 147)), ("DeepSkyBlue", (RGB 0 191 255)), ("DimGray", (RGB 105 105 105)), ("DimGrey", (RGB 105 105 105)),
                  ("DodgerBlue", (RGB 30 144 255)), ("FireBrick", (RGB 178 34 34)), ("FloralWhite", (RGB 255 250 240)), ("ForestGreen", (RGB 34 139 34)),
                  ("Fuchsia", (RGB 255 0 255)), ("Gainsboro", (RGB 220 220 220)), ("GhostWhite", (RGB 248 248 255)), ("Gold", (RGB 255 215 0)),
                  ("GoldenRod", (RGB 218 165 32)), ("Gray", (RGB 128 128 128)), ("Grey", (RGB 128 128 128)), ("Green", (RGB 0 128 0)),
                  ("GreenYellow", (RGB 173 255 47)), ("HoneyDew", (RGB 240 255 240)), ("HotPink", (RGB 255 105 180)), ("IndianRed", (RGB 205 92 92)),
                  ("Indigo", (RGB 75 0 130)), ("Ivory", (RGB 255 255 240)), ("Khaki", (RGB 240 230 140)), ("Lavender", (RGB 230 230 250)),
                  ("LavenderBlush", (RGB 255 240 245)), ("LawnGreen", (RGB 124 252 0)), ("LemonChiffon", (RGB 255 250 205)), ("LightBlue", (RGB 173 216 230)),
                  ("LightCoral", (RGB 240 128 128)), ("LightCyan", (RGB 224 255 255)), ("LightGoldenRodYellow", (RGB 250 250 210)),
                  ("LightGray", (RGB 211 211 211)), ("LightGrey", (RGB 211 211 211)), ("LightGreen", (RGB 144 238 144)), ("LightPink", (RGB 255 182 193)),
                  ("LightSalmon", (RGB 255 160 122)), ("LightSeaGreen", (RGB 32 178 170)), ("LightSkyBlue", (RGB 135 206 250)), ("LightSlateGray", (RGB 119 136 153)),
                  ("LightSlateGrey", (RGB 119 136 153)), ("LightSteelBlue", (RGB 176 196 222)), ("LightYellow", (RGB 255 255 224)), ("Lime", (RGB 0 255 0)),
                  ("LimeGreen", (RGB 50 205 50)), ("Linen", (RGB 250 240 230)), ("Magenta", (RGB 255 0 255)), ("Maroon", (RGB 128 0 0)), ("MediumAquaMarine", (RGB 102 205 170)),
                  ("MediumBlue", (RGB 0 0 205)), ("MediumOrchid", (RGB 186 85 211)), ("MediumPurple", (RGB 147 112 219)), ("MediumSeaGreen", (RGB 60 179 113)),
                  ("MediumSlateBlue", (RGB 123 104 238)), ("MediumSpringGreen", (RGB 0 250 154)), ("MediumTurquoise", (RGB 72 209 204)), ("MediumVioletRed", (RGB 199 21 133)),
                  ("MidnightBlue", (RGB 25 25 112)), ("MintCream", (RGB 245 255 250)), ("MistyRose", (RGB 255 228 225)), ("Moccasin", (RGB 255 228 181)), ("NavajoWhite", (RGB 255 222 173)),
                  ("Navy", (RGB 0 0 128)), ("OldLace", (RGB 253 245 230)), ("Olive", (RGB 128 128 0)), ("OliveDrab", (RGB 107 142 35)), ("Orange", (RGB 255 165 0)),
                  ("OrangeRed", (RGB 255 69 0)), ("Orchid", (RGB 218 112 214)), ("PaleGoldenRod", (RGB 238 232 170)), ("PaleGreen", (RGB 152 251 152)),
                  ("PaleTurquoise", (RGB 175 238 238)), ("PaleVioletRed", (RGB 219 112 147)), ("PapayaWhip", (RGB 255 239 213)), ("PeachPuff", (RGB 255 218 185)),
                  ("Peru", (RGB 205 133 63)), ("Pink", (RGB 255 192 203)), ("Plum", (RGB 221 160 221)), ("PowderBlue", (RGB 176 224 230)),
                  ("Purple", (RGB 128 0 128)), ("RebeccaPurple", (RGB 102 51 153)), ("Red", (RGB 255 0 0)), ("RosyBrown", (RGB 188 143 143)),
                  ("RoyalBlue", (RGB 65 105 225)), ("SaddleBrown", (RGB 139 69 19)), ("Salmon", (RGB 250 128 114)), ("SandyBrown", (RGB 244 164 96)),
                  ("SeaGreen", (RGB 46 139 87)), ("SeaShell", (RGB 255 245 238)), ("Sienna", (RGB 160 82 45)), ("Silver", (RGB 192 192 192)),
                  ("SkyBlue", (RGB 135 206 235)), ("SlateBlue", (RGB 106 90 205)), ("SlateGray", (RGB 112 128 144)), ("SlateGrey", (RGB 112 128 144)),
                  ("Snow", (RGB 255 250 250)), ("SpringGreen", (RGB 0 255 127)), ("SteelBlue", (RGB 70 130 180)), ("Tan", (RGB 210 180 140)),
                  ("Teal", (RGB 0 128 128)), ("Thistle", (RGB 216 191 216)), ("Tomato", (RGB 255 99 71)), ("Turquoise", (RGB 64 224 208)),
                  ("Violet", (RGB 238 130 238)), ("Wheat", (RGB 245 222 179)), ("White", (RGB 255 255 255)), ("WhiteSmoke", (RGB 245 245 245)),
                  ("Yellow", (RGB 255 255 0)), ("YellowGreen", (RGB 154 205 50)) ]

-- TODO:: cannot use this
allLower :: String -> String
allLower []  = []
allLower str = [ toLower s | s <- str]

-- Apply chain min or max to 3 elements
tripletOrd :: Ord a => (a -> a -> a) -> a -> a -> a -> a
tripletOrd f x y z = f (f x y) z

-- In the hue calculation equation 60 x <deltaHelper>
-- calculates the part of deltaHelper
deltaHelper :: Float -> Int -> Int -> Float
deltaHelper d x y = (fromIntegral (x - y) / 255.0) / d


calculateH :: Float -> Int -> Int -> Int -> Float
calculateH 0.0 _ _ _ = 0
calculateH delta r g b
  | cmax == r = 60.0 * (mod' (deltaHelper delta g b) 6.0)
  | cmax == g = 60 * (deltaHelper delta b r + 2)
  | cmax == b = 60 * (deltaHelper delta r g + 4)
  where
    cmax = tripletOrd max r g b

calculateS :: Float -> Float -> Float
calculateS 0    delta = 0
calculateS cmax delta = delta / cmax

normRGB :: Float -> Float -> Int
normRGB x m = round ((x + m) * 255)

cxmToRGB :: Float -> Float -> Float -> Float -> Color
cxmToRGB c x m h
  | 0   <= h && h < 60  =  RGB (normRGB c m) (normRGB x m) (normRGB 0 m)
  | 60  <= h && h < 120 =  RGB (normRGB x m) (normRGB c m) (normRGB 0 m)
  | 120 <= h && h < 180 =  RGB (normRGB 0 m) (normRGB c m) (normRGB x m)
  | 180 <= h && h < 240 =  RGB (normRGB 0 m) (normRGB x m) (normRGB c m)
  | 240 <= h && h < 300 =  RGB (normRGB x m) (normRGB 0 m) (normRGB c m)
  | 300 <= h && h < 360 =  RGB (normRGB c m) (normRGB 0 m) (normRGB x m)


rgb2hsv :: Color -> Color
rgb2hsv (RGB r g b) = HSV (calculateH delta r g b) (calculateS cmax delta) cmax
  where
    cmax = ( fromIntegral (tripletOrd max r g b) ) / 255.0
    cmin = ( fromIntegral (tripletOrd min r g b) ) / 255.0
    delta = cmax - cmin

hsv2rgb :: Color -> Color
hsv2rgb (HSV h s v) = cxmToRGB c x m h
  where
    c = v * s
    x = c * (1 - (abs  (mod' (h / 60) 2 - 1)))
    m = v - c

name2rgb :: String -> Color
name2rgb str = findName str htmlColors
  where
    findName :: String ->  [ (String, Color) ] -> Color
    findName str [] = error "No such color name"
    findName str (x:xs) = if str == fst x then snd x else findName str xs

hsvGradient :: Color -> Color -> Int -> [Color]
hsvGradient (HSV h1 s1 v1) (HSV h2 s2 v2) step = zipWith3 HSV hs ss vs
  where
    hStep = ((h2 - h1) / (fromIntegral step))
    sStep = ((s2 - s1) / (fromIntegral step))
    vStep = ((v2 - v1) / (fromIntegral step))
    hs = [h1 + hStep* fromIntegral n | n <- [0..step] ]
    ss = [s1 + sStep* fromIntegral n | n <- [0..step] ]
    vs = [v1 + vStep* fromIntegral n | n <- [0..step] ]

hueDesc :: Float -> String
hueDesc h
  | h > 344   = "red "
  | h > 327   = "rose "
  | h > 291   = "magenta "
  | h > 270   = "purple "
  | h > 260   = "violet "
  | h > 240   = "indigo "
  | h > 193   = "blue "
  | h > 163   = "cyan "
  | h > 79    = "green "
  | h > 70    = "lime "
  | h > 45    = "yellow "
  | h > 15.1  = "orange "
  | h > 14.9  = "reddish "
  | h < 15    = "red "

satDes :: Float -> String
satDes s
  | s > 0.9   = "very saturated "
  | s > 0.8   = "rather saturated "
  | s > 0.6   = "saturated "
  | s > 0.46  = "rather unsaturated "
  | s > 0.3   = "unsaturated "
  | s > 0.1   = "very unsaturated "
  | s > 0.03  = "almost grey "
  | otherwise = "grey "


lDesc :: Float -> String
lDesc l
  | l > 0.94  = "almost white "
  | l > 0.8   = "very light "
  | l > 0.6   = "light "
  | l > 0.3   = "normal "
  | l > 0.22  = "dark "
  | l > 0.09  = "very dark "
  | otherwise = "almost black "

hsv2desc :: Color -> IO()
hsv2desc (HSV h s v) =  putStrLn (hueDesc h ++ satDes s ++ lDesc l)
  where
    RGB r g b = hsv2rgb $ HSV h s v
    l =  fromIntegral ((tripletOrd max r g b) + (tripletOrd min r g b)) / 510

-- Finds the name of the color which is closest to given color values
findClosest :: Color -> String
findClosest (RGB r g b) =  fst $ foldl1 (whichOneCloser $ RGB r g b) htmlColors
findClosest (HSV h s v) =  fst $ foldl1  (whichOneCloser $ hsv2rgb $ HSV h s v) htmlColors

-- For a given 2 htmlcolor element returns the closer one to given color
whichOneCloser :: Color -> (String,Color) -> (String,Color) -> (String,Color)
whichOneCloser (RGB r1 g1 b1) (str, (RGB r2 g2 b2)) (str2, (RGB r3 g3 b3)) =
  if calculateHSVDist (rgb2hsv $ RGB r1 g1 b1) (rgb2hsv $ RGB r2 g2 b2) >
    calculateHSVDist (rgb2hsv $ RGB r1 g1 b1) (rgb2hsv $ RGB r3 g3 b3)
    then (str2, (RGB r3 g3 b3))
    else (str, (RGB r2 g2 b2))
whichOneCloser (HSV h s v)  (str, (HSV h2 s2 v2)) (str2, (HSV h3 s3 v3)) =
  if calculateHSVDist (HSV h s v) (HSV h2 s2 v2) >
    calculateHSVDist (HSV h s v) (HSV h3 s3 v3)
  then (str2, (HSV h3 s3 v3))
  else (str, (HSV h2 s2 v2))

-- Calculates the color difference in HSV representation
calculateHSVDist :: Color -> Color -> Float
calculateHSVDist (HSV h s v) (HSV h2 s2 v2) = (abs (h - h2) ) / 360 +
                                              (abs (s - s2) ) +
                                              (abs (v - v2) )
