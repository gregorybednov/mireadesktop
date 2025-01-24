import java.util.*


typealias Menu = HashMap<String, Pair<String, MutableList<String>>>

val deleteSubmenus = arrayOf("apps-dir-Education", "apps-dir-Settings")
val newSubmenus = arrayOf(Pair("apps-dir-Database", "БД и проект-ие"))

/// Добавляет новые записи как есть, в формате CSV для jgmenu_run
val appendix: MutableList<Pair<String, String>> = mutableListOf(
    Pair("Archi (Archimate Modeling Tool),Archi,archi,,#Education", "apps-dir-Database"),
    Pair("Выключить,powermenu poweroff,,,#System", "apps-dir-System"), // TODO powermenu
    Pair("Перезагрузить,powermenu reboot,,,#System", "apps-dir-System"), // TODO powermenu
    Pair("IntelliJ IDEA Community Edition,idea-community,idea-community,,#Development", "apps-dir-Development")
)

/// Удаляет записи, содержащие строку1, из всех подменю при строке2==null,
/// иначе только из подменю с тегом из переменной строка2
val deletions: MutableList<Pair<String, String?>> = mutableListOf(
    Pair("XTerm", null),
    Pair("Vim", null),
    Pair("LibreOffice Draw", null),
    Pair("LibreOffice Math", null),
    Pair("LibreOffice Base", null),
    Pair("Tint2", null),
    Pair("(Free Java", null)
)

/// Добавляет новые "перемещения" (удаления из одного подменю и добавления в другое)
val moves: Array<Triple<String, String, String>> = arrayOf(
    Triple("Camunda Modeler", "apps-dir-Other", "apps-dir-Database"),
    Triple("StarUML", "apps-dir-Development", "apps-dir-Database"),
    Triple("MySQL Workbench", "apps-dir-Development", "apps-dir-Database")
)

fun readWhile(m: Scanner, p: (String) -> Boolean):
        MutableList<String> {
    val l: MutableList<String> = mutableListOf()
    while (m.hasNext()) {
        val s = m.nextLine()
        if (! p(s)) return l
        l.addLast(s!!)
    }
    return l
}

fun readMenu (m: Scanner): Menu {
    fun String.erase(m: Regex): String {
        return this.replace(m, "")
    }

    fun regexDumbTriple(name: String): Triple<Regex, Regex, Regex> {
        val left = Regex("\\^$name\\(")
        val right = Regex("\\)")
        val tag = Regex("^$left[^)]*$right$")
        return Triple(left, right, tag)
    }

    val menu = HashMap<String, Pair<String, MutableList<String>>>()
    val heads = readWhile(m) { it != "" }

    for (s in heads) {
        val (l, r, _) = regexDumbTriple("checkout")

        val lst = s.split(",")
        val key = lst[1].erase(l).erase(r)
        val nam = lst[0]
        menu[key] = Pair(nam, mutableListOf())
    }

    val tails = readWhile(m) { true }
    var currentTagName = ""
    for (s in tails) {
        val (l, r, t) = regexDumbTriple("tag")

        when {
            s.matches(t) ->
                currentTagName = s.erase(l).erase(r)
            s.isEmpty() -> continue
            else ->
                menu[currentTagName]?.second?.addLast(s)
        }
    }

    return menu
}

fun findEntries (what: String, where: Pair<String, MutableList<String>>): List<String> {
    return where.second.filter { it.contains(what) }
}

fun printMenu (menu: Menu) {
    menu.map {
        val tag = it.key
        val caption = it.value.first
        println("$caption,^checkout($tag),applications-$tag")
    }
    println()
    menu.map { submenu ->
        val tag = submenu.key
        val entries = submenu.value.second
        if (entries.isNotEmpty()){
            println("^tag($tag)")
            entries.map { println(it) }
            println()
        }
    }
}

fun main() {
    val scanStdin = Scanner(System.`in`)
    val menu = readMenu(scanStdin)

    deleteSubmenus.forEach { menu.remove(it) }
    newSubmenus.forEach {
        menu[it.first] = Pair(it.second, mutableListOf())
    }

    for (m in moves) {
        if (menu.containsKey(m.second)) {
            val w = menu[m.second]?.let { findEntries(m.first, it) }
            if (w != null) {
                deletions.addAll(w.map { Pair(it, m.second) })
                appendix.addAll(w.map { Pair(it, m.third) })
            }
        }
    }

    for (d in deletions) {
        if (d.second == null) {
            menu.map { submenu ->
                submenu.value.second.removeIf { it.contains(d.first) }
            }
        } else {
            menu[d.second]?.second?.removeIf { it.contains(d.first) }
        }
    }

    for (a in appendix) {
        menu[a.second]?.second?.addLast(a.first)
    }

    printMenu(menu)

}
