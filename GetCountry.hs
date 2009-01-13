{-# LANGUAGE PatternGuards #-}
module GetCountry where

split :: Char -> String -> [String]
split _ [] = []
split g xs = y : split g ys
  where (y,ys) = split2 g xs
        (ys',zs) = break (==g) xs
        split2 g xs = (ys', drop 1 zs)

readInt :: String -> Integer
readInt = read . unpack

readIntT :: String -> [Int]
readIntT ip = let l = split '.' ip
              in map readInt' l

readInt' :: String -> Int
readInt' = read

unpack = init . tail

database = file filepath >>= return . trata
filepath = "IpToCountry.csv"
file path = readFile path >>= return . map (split ';') . lines

trata = foldr (\[a,b,c,d,_] cs -> (readInt a, readInt b, unpack d) : cs) []

getCountryByIP' :: String -> [(Integer,Integer,String)] -> IO String
getCountryByIP' _ [] = return []
getCountryByIP' ip ((from,to,country):cs)
        | from <= ip' , ip' <= to = return country
        | otherwise = getCountryByIP' ip cs
    where [a,b,c,d] = readIntT ip
          ip' = toInteger (d + 256*c + 256*256*b + 256*256*256*a)

getCountryByIP ips = database >>= getCountryByIP' ips
