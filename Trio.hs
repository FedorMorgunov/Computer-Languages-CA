import Data.List (nub, permutations)

generator2 :: [(String, String, String, String, String)]
generator2 =
    [ (num1, num2, num3, num4, num5)
    | num1 <- map show [123..987],
      '0' `notElem` num1,
      let perms1 = nub (permutations num1),
      num2 <- map (take 2) perms1,
      '0' `notElem` num2,
      num3 <- perms1,
      '0' `notElem` num3,
      num4 <- map (take 2) perms1,
      '0' `notElem` num4,
      num5 <- perms1,
      '0' `notElem` num5,
      head num1 /= head num2
    ]

x_generator2 :: Int
x_generator2 =
  length [t | t <- ts, t `elem` g]
  where
    g = generator2
    ts =
      [ ("123", "21", "123", "12", "123")
      , (" 162", "26", "261 ", "12", "621 ")
      , (" 219", "19", "912 ", "21", "291 ")
      , (" 329", "92", "932 ", "32", "239 ")
      , (" 439", "94", "394 ", "43", "394 ")
      , (" 549", "95", "945 ", "95", "945 ")
      , (" 568", "68", "586 ", "56", "586 ")
      , (" 769", "67", "679 ", "97", "796 ")
      , (" 879", "79", "897 ", "98", "789 ")
      , (" 987", "79", "789 ", "79", "789 ")
      ]

tester2 :: ([Char], [Char], [Char], [Char], [Char]) -> Bool
tester2 (n1, n2, n3, n4, n5) =
  let cond1 = read n1 - read n2 == read n3
      cond2 = read n3 - read n4 == read n5
      cond3 = read n1 + read n3 + read n5 < 2000
  in cond1 && cond2 && cond3

x_tester2 :: Int
x_tester2 =
  length [t | t <- ts, tester2 t]
  where
    ts =
      [ ("138", "01", "137", "50", "87")
      , ("143", "01", "142", "52", "90")
      , ("171", "02", "169", "79", "90")
      , ("152", "03", "149", "54", "95")
      , ("159", "04", "155", "61", "94")
      , ("161", "05", "156", "63", "93")
      , ("182", "06", "176", "80", "96")
      , ("151", "07", "144", "57", "87")
      , ("165", "08", "157", "64", "93")
      , ("174", "09", "165", "71", "94")
      ]

main :: IO ()
main = do
  let solutions = filter tester2 generator2
  print solutions