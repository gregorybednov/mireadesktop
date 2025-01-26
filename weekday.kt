import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Month

enum class Period {
    Autumn,
    Winter,
    Spring
}

fun determinePeriod(today: LocalDate): Period {
    val februaryCutoff = LocalDate.of(today.year, Month.FEBRUARY, 9)
    val septemberCutoff = LocalDate.of(today.year, Month.SEPTEMBER, 1)

    return when {
        today.isBefore(februaryCutoff) -> Period.Winter
        today.isAfter(septemberCutoff.minusDays(1)) -> Period.Autumn
        else -> Period.Spring
    }
}

fun calculateWeek(period: Period, date: LocalDate): String {
    return when {
        period == Period.Winter -> "Хороших праздников, удачной сессии!"
        date.dayOfWeek == DayOfWeek.SUNDAY -> "Сегодня воскресенье, лучше иди домой"
        else -> {
            val currentWeek = date.getWeekOfYear()
            val periodLimit = when (period) {
                Period.Spring -> LocalDate.of(date.year, Month.FEBRUARY, 9)
                Period.Autumn -> LocalDate.of(date.year, Month.SEPTEMBER, 1)
                else -> throw IllegalStateException("Unexpected period")
            }
            val limitWeek = periodLimit.getWeekOfYear()
            val isLimitSunday = periodLimit.dayOfWeek == DayOfWeek.SUNDAY

            val weekOffset = if (isLimitSunday) 1 else 0
            val weekNumber = 1 + currentWeek - limitWeek - weekOffset
            "$weekNumber неделя"
        }
    }
}

fun LocalDate.getWeekOfYear(): Int {
    return this.get(java.time.temporal.ChronoField.ALIGNED_WEEK_OF_YEAR)
}

fun main() {
    val today = LocalDate.now()
    val period = determinePeriod(today)
    val weekInfo = calculateWeek(period, today)
    println(weekInfo)
}
