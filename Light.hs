generator1 :: [(Int, Int, Int, Int)]
generator1 = [(hr, mn, dy, mt) | hr <- [0..23], mn <- [0..59], mt <- [1..12], dy <- [1..daysInMonth mt], isValidDate hr mn dy mt]
  where
    isValidDate hr mn dy mt
        | hr >= 0 && hr <= 23 &&
          mn >= 0 && mn <= 59 &&
          dy >= 1 && dy <= daysInMonth mt = True
        | otherwise = False

    daysInMonth :: Int -> Int
    daysInMonth 2 = 28 -- February
    daysInMonth m
        | m `elem` [4, 6, 9, 11] = 30
        | otherwise = 31

litSegments :: Int -> Int
litSegments digit = case digit of
  0 -> 6
  1 -> 2
  2 -> 5
  3 -> 5
  4 -> 4
  5 -> 5
  6 -> 6
  7 -> 3
  8 -> 7
  9 -> 6
  otherwise -> 0

totalLitSegments :: (Int, Int, Int, Int) -> Int
totalLitSegments (hr, mn, dy, mt) =
  sum [litSegments d | d <- int_generator2 [hr, mn, dy, mt]]

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | n <= 3 = True
  | n `mod` 2 == 0 || n `mod` 3 == 0 = False
  | otherwise = isPrime' n 5
  where
    isPrime' n i
      | i * i > n = True
      | n `mod` i == 0 || n `mod` (i + 2) == 0 = False
      | otherwise = isPrime' n (i + 6)

isMagic :: (Int, Int, Int, Int) -> Bool
isMagic (hr, mn, dy, mt) =
  areDigitsDifferent [hr, mn, dy, mt] && isPrime (totalLitSegments (hr, mn, dy, mt))

areDigitsDifferent :: [Int] -> Bool
areDigitsDifferent integers = hasNoDuplicates (int_generator2 integers)

hasNoDuplicates :: [Int] -> Bool
hasNoDuplicates [] = True -- An empty list has no duplicates
hasNoDuplicates (x:xs)
  | x `elem` xs = False -- If x is in the rest of the list, there is a duplicate
  | otherwise = hasNoDuplicates xs -- Check the rest of the list


int_generator2 :: [Int] -> [Int]
int_generator2 [] = []
int_generator2 (x:xs)
  | x < 10 = 0 : x : int_generator2 xs  -- Add a '0' before single-digit numbers
  | otherwise = (x `div` 10) : (x `mod` 10) : int_generator2 xs  -- Split two-digit numbers


nextMagicTuple :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
nextMagicTuple (hr, mn, dy, mt) =
  if dy < daysInMonth mt
    then (hr, mn, dy + 1, mt)  -- Simply add one day
    else if mt < 12
      then (hr, mn, 1, mt + 1)  -- Move to the first day of the next month
      else (hr, mn, 1, 1)      -- If it's December, move to the first day of January
  where
    daysInMonth 2 = 28  -- February
    daysInMonth m
        | m `elem` [4, 6, 9, 11] = 30  -- Months with 30 days
        | otherwise = 31  -- All other months have 31 days

nextMinuteTuple :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
nextMinuteTuple (hr, mn, dy, mt) =
  if mn == 59
    then if hr == 23
      then if dy == daysInMonth mt
        then if mt == 12
          then (0, 0, 1, 1)  -- Reset hour, minute, day, and month, increment year
          else (0, 0, 1, mt + 1)  -- Reset hour, minute, day, and increment month
        else (0, 1, dy + 1, mt)  -- Reset hour and minute, increment day
      else (hr + 1, 0, dy, mt)  -- Increment hour, reset minute
    else (hr, mn + 1, dy, mt)  -- Increment minute

daysInMonth :: Int -> Int
daysInMonth 2 = 28  -- February
daysInMonth m
  | m `elem` [4, 6, 9, 11] = 30  -- Months with 30 days
  | otherwise = 31  -- All other months have 31 days


tester1 :: (Int, Int, Int, Int) -> Bool
tester1 (hr, mn, dy, mt) =
  let firstTuple = (hr, mn, dy, mt)
      secondTuple = nextMagicTuple firstTuple
      thirdTuple = nextMinuteTuple secondTuple
      avgLitSegments = (totalLitSegments firstTuple + totalLitSegments secondTuple) `div` 2
  in isMagic firstTuple && isMagic secondTuple && totalLitSegments thirdTuple == avgLitSegments

x_tester1 :: Int
x_tester1 = 
    length [t | t <- ts, tester1 t]
    where
    ts =
        [ (6, 59, 17, 24)
        , (6, 59, 17, 34)
        , (6, 59, 27, 14)
        , (6, 59, 27, 41)
        , (8, 59, 12, 46)
        , (16, 59, 7, 24)
        , (16, 59, 7, 42)
        , (16, 59, 7, 43)
        , (16, 59, 27, 40)
        , (18, 59, 2, 46)
        ]

x_generator1 :: Int
x_generator1 = length [t | t <- ts, t `elem` g]
  where
    g = generator1
    ts =
      [ (2, 15, 14, 11)
      , (4, 31, 27, 9)
      , (6, 47, 10, 8)
      , (9, 3, 23, 6)
      , (11, 19, 6, 5)
      , (13, 35, 19, 3)
      , (15, 51, 2, 2)
      , (18, 6, 16, 12)
      , (20, 22, 29, 10)
      , (22, 38, 11, 9)
      ]

main :: IO ()
main = do
    let result = filter tester1 generator1
    print result