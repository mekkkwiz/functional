-- Q: define type Month whose values are months in a year?
data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show)

-- Q: implement function
-- daysInMonth :: Month -> Integer
    -- daysInMonth February → 28
daysInMonth :: Month -> Integer
daysInMonth month = case month of
    January   -> 31
    February  -> 28
    March     -> 31
    April     -> 30
    May       -> 31
    June      -> 30
    July      -> 31
    August    -> 31
    September -> 30
    October   -> 31
    November  -> 30
    December  -> 31


-- aux functions for data type Month
monthToIndex :: Month -> Int
monthToIndex month = case month of
    January   -> 1
    February  -> 2
    March     -> 3
    April     -> 4
    May       -> 5
    June      -> 6
    July      -> 7
    August    -> 8
    September -> 9
    October   -> 10
    November  -> 11
    December  -> 12

indexToMonth :: Int -> Month
indexToMonth index = case index of
    1  -> January
    2  -> February
    3  -> March
    4  -> April
    5  -> May
    6  -> June
    7  -> July
    8  -> August
    9  -> September
    10 -> October
    11 -> November
    12 -> December
    _ -> error "invalid index"

-- Q: implement function
-- nextDay :: Integer -> Month -> (Integer, Month)
-- that takes date/month and returns the following date/month
    -- nextDay 11 January → (12, January)
    -- nextDay 28 February → (1, March)
    -- nextDay 31 December → (1, January)


nextDay :: Integer -> Month -> (Integer, Month)
nextDay day month =
    if day < daysInMonth month
    then (day + 1, month)
    else (1, nextMonth month)

nextMonth :: Month -> Month
nextMonth month =
    if show month == "December"
    then January
    else indexToMonth ((monthToIndex month) + 1)
