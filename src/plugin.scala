// Домашнее Задание
// В форке проекта `scala-bootcamp` или любом другом проекте определите автоплагин `BulkySourcesPlugin` содержащий
//     setting `bulkyThresholdInLines` типа `SettingKey[Int]` по умолчанию = 100
//     task    `bulkySources` типа `TaskKey[Seq[(Int, File)]]`
// Реализуйте `bulkySources` для основных и тестовых исходников чтобы вывод
//    `show bulkySources` показал список “больших” исходников с количеством строк
//    более или равным настройке `bulkyThresholdInLines` отсортированным по убыванию размера.
//     К примеру `show bulkySources` должен выдать подобное
//     ```
//     [info] * (130, .../src/main/scala/../../../A.scala)
//     [info] * (100, .../src/main/scala/../../../B.scala)
//     ```
//     А `show test:bulkySources`
//     ```
//     [info] * (170, .../src/test/scala/../../../Y.scala)
//     [info] * (110, .../src/test/scala/../../../Z.scala)
//     ```
// Какую область видимости стоит выбрать для настройки и задачи - Global/ThisBuild/Project ?
// Все файлы основных исходников можно найти используя sbt console `show compile:sources`, а тестовых - `show test:sources`. По выводу можно определить конфигурации для использования написания плагина.
// Определить кол-во строк в файле можно при помощи `sbt.IO` или другого удобного способа.
// По желанию
// Оформите этот плагин как отдельный проект
