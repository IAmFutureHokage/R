sink("Дорошенко.out")

# Определение функции
interval_histogram <- function(x, k = length(x)/10) {
  # Округление k вниз до ближайшего целого числа
  k <- floor(k)
  
  # Вычисление минимального и максимального значений вектора
  xmin <- min(x)
  xmax <- max(x)
  
  # Разбиение диапазона на k интервалов
  breaks <- seq(xmin, xmax, length.out = k + 1)
  
  # Подсчет числа элементов вектора в каждом интервале
  counts <- hist(x, breaks = breaks, plot = FALSE)$counts
  
  # Вычисление середин интервалов
  midpoints <- (breaks[-(k + 1)] + breaks[-1]) / 2
  
  # Нормализация числа элементов вектора в каждом интервале
  normalized_counts <- counts / length(x)
  
  # Построение графика
  plot(midpoints, normalized_counts, type = "b", xlab = "Midpoints", ylab = "Frequency", main = "Interval Histogram")
}

# Генерация вектора длины 5000 из нормального распределения
x <- rnorm(5000)

# Вызов функции
interval_histogram(x)


sink();





