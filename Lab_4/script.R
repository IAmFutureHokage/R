sink("Дорошенко.out")

#1.	Данные из файла (данные.xlsx): в колонках «C-4G» и «C-6G» 
#генотипы исследуемых, «C-4» и «C-6» - уровень соответствующего цитокина

library(readxl)

# Загрузка данных
data <- read_excel("data/данные.xlsx")

# Преобразование данных с запятыми в числа с точками
data$`C-4` <- as.numeric(gsub(",", ".", data$`C-4`))
data$`C-6` <- as.numeric(gsub(",", ".", data$`C-6`))

# Поиск и исправление возможных опечаток в генотипах
data$`C-4G` <- as.character(data$`C-4G`)
data$`C-6G` <- as.character(data$`C-6G`)

# Замена кириллических символов "С" и "Т" на латинские "C" и "T"
data$`C-4G` <- chartr(old = "СТ", new = "CT", x = data$`C-4G`)
data$`C-6G` <- chartr(old = "СТ", new = "CT", x = data$`C-6G`)

# Преобразование символьных данных обратно в факторы
data$`C-4G` <- as.factor(data$`C-4G`)
data$`C-6G` <- as.factor(data$`C-6G`)

# Преобразование данных в символьный формат
data$`C-4` <- as.character(data$`C-4`)
data$`C-6` <- as.character(data$`C-6`)

# Применение функции gsub только к значениям больше 10
data$`C-4` <- ifelse(as.numeric(data$`C-4`) > 10, 
                     gsub("(\\d)(\\d)$", "\\1.\\2", data$`C-4`), 
                     data$`C-4`)
data$`C-6` <- ifelse(as.numeric(data$`C-6`) > 10, 
                     gsub("(\\d)(\\d)$", "\\1.\\2", data$`C-6`), 
                     data$`C-6`)

# Преобразование данных обратно в числовой формат
data$`C-4` <- as.numeric(data$`C-4`)
data$`C-6` <- as.numeric(data$`C-6`)

# Стандартный график для C-4
plot(data$`C-4` ~ data$`C-4G`, xlab = "Genotype C-4G", ylab = "Cytokine Level C-4")

# Стандартный график для C-6
plot(data$`C-6` ~ data$`C-6G`, xlab = "Genotype C-6G", ylab = "Cytokine Level C-6")

# Параметрические характеристики для C-4
mean_C4 <- mean(data$`C-4`, na.rm = TRUE)  # Среднее значение уровня цитокина C-4
sd_C4 <- sd(data$`C-4`, na.rm = TRUE)      # Стандартное отклонение уровня цитокина C-4

# Параметрические характеристики для C-6
mean_C6 <- mean(data$`C-6`, na.rm = TRUE)  # Среднее значение уровня цитокина C-6
sd_C6 <- sd(data$`C-6`, na.rm = TRUE)      # Стандартное отклонение уровня цитокина C-6

# Непараметрические характеристики для C-4
median_C4 <- median(data$`C-4`, na.rm = TRUE)  # Медиана уровня цитокина C-4
iqr_C4 <- IQR(data$`C-4`, na.rm = TRUE)        # Межквартильный размах уровня цитокина C-4

# Непараметрические характеристики для C-6
median_C6 <- median(data$`C-6`, na.rm = TRUE)  # Медиана уровня цитокина C-6
iqr_C6 <- IQR(data$`C-6`, na.rm = TRUE)        # Межквартильный размах уровня цитокина C-6


#########################################################################################################

data <- read.table("data/group.txt", header = TRUE)

# Преобразование в факторы, чтобы использовать их для изменения формы и цвета точек на графике
data$Gender <- as.factor(data$Gender)
data$Clothing_Size <- factor(data$Clothing_Size, levels = c("S", "M", "L"))

# Создание графика снова
plot(data$Height, data$Weight, 
     pch = 2 + as.numeric(data$Clothing_Size),  # Изменение формы точек в зависимости от размера одежды
     col = ifelse(data$Gender == "M", "blue", "red"),  # Изменение цвета точек в зависимости от пола
     xlab = "Рост (см)", 
     ylab = "Вес (кг)",
     main = "График зависимости роста от веса")

# Добавление легенды для указания, что означают формы и цвета
legend("topright", 
       legend = levels(data$Clothing_Size),
       title = "Размер одежды",
       pch = 2 + as.numeric(data$Clothing_Size))

legend("bottomright",
       legend = c("Мужской", "Женский"),
       title = "Пол",
       col = c("blue", "red"),
       pch = 1)  # Использование pch = 1 для обозначения круглых точек в легенде

sink();





