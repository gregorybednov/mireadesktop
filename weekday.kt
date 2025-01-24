import java.time.LocalDate
import java.time.DayOfWeek
import java.time.Month
import java.time.format.DateTimeFormatter

enum class Period {
    Autumn,
    Winter,
    Spring
}

fun period(thisYear: (Month) -> LocalDate, today: LocalDate): Period {
    return when {
        today.isBefore(thisYear(Month.FEBRUARY).plusDays(9)) -> Period.Winter
        today.isAfter(thisYear(Month.SEPTEMBER).minusDays(1)) -> Period.Autumn
        else -> Period.Spring
    }
}

fun week(p: Period, d: LocalDate): String {
    return when (p) {
        Period.Winter -> "Хороших праздников, удачной сессии!"
        else -> {
            if (d.dayOfWeek == DayOfWeek.SUNDAY) {
                "Сегодня воскресенье, лучше иди домой"
            } else {
                val (y, x0) = toWeekDate(d)
                val limit = if (p == Period.Spring) thisYear(Month.FEBRUARY).plusDays(9) else thisYear(Month.SEPTEMBER).plusDays(1)
                val (_, x1) = toWeekDate(limit)
                val limitIsSunday = limit.dayOfWeek == DayOfWeek.SUNDAY
                val x = 1 + x0 - x1 - if (limitIsSunday) 1 else 0
                "$x неделя"
            }
        }
    }
}

fun toWeekDate(date: LocalDate): Pair<Int, Int> {
    val weekOfYear = date.get(java.time.temporal.ChronoField.ALIGNED_WEEK_OF_YEAR)
    val year = date.year
    return Pair(year, weekOfYear.toInt())
}

fun thisYear(month: Month): LocalDate {
    val now = LocalDate.now()
    return LocalDate.of(now.year, month, 1)
}

fun main() {
    val today = LocalDate.now()
    val (year, _, _) = today.toGregorian()
    println(week(period(::thisYear, today), today))
}

fun LocalDate.toGregorian(): Triple<Int, Int, Int> {
    return Triple(this.year, this.monthValue, this.dayOfMonth)
}
