import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)

data Period = Autumn
            | Winter
            | Spring
            deriving (Show, Eq)

period :: (Ord a, Num t) => (MonthOfYear -> t -> a) -> a -> Period
period thisYear today
    | today < thisYear February 9 = Winter
    | today >= thisYear September 1 = Autumn
    | otherwise = Spring 

week :: Period -> Day -> String
week Winter _ = "Хороших праздников, удачной сессии!"
week p d
    | dayOfWeek d == Sunday = "Сегодня воскресенье, лучше иди домой"
    | otherwise = show x ++ " неделя" where
                        x = 1 + x0 - x1 - if limitIsSunday then 1 else 0
                        limitIsSunday = dayOfWeek limit == Sunday 
                        (_, x1, _) = toWeekDate limit
                        (y, x0, _) = toWeekDate d
                        limit = if p == Spring
                            then this February 9
                            else this September 1
                        this = fromGregorian y
main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    --let today = fromGregorian 2024 September 9 -- it was TEST
    let (year,_,_) = toGregorian today
    putStr $ week (period (fromGregorian year) today) today