main = do
  putStrLn $ "1: " ++ (show q1)
  putStrLn $ "2: " ++ (show q2)
  putStrLn $ "3: " ++ (show q3)
  putStrLn $ "4: " ++ (show q4)
  putStrLn $ "5: " ++ (show q5)

q1 :: Int
q1 = sum [ x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0 ]

fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
q2 :: Int
q2 = sum [ x | x <- takeWhile (<4000000) fibs, mod x 2 == 0 ]

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [ x | x <- xs, x `mod` p /= 0 ]
q3 :: Int
q3 = last [ x | x <- [2..6857], mod 600851475143 x == 0, elem x $ take 775147 primes ]

mult x = map (*x) [100..999]
q4 :: Int
q4 = maximum $ filter (\x -> reverse (show x) == (show x)) (concat (map (mult) [100..999]))

q5 = foldr1 lcm [1..20]
