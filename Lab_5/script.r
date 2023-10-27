sink("Дорошенко.out")

library(boot)

# Задание 1

# Данные
salary <- c(21, 19, 27, 11, 102, 25, 21)
n <- length(salary) # Кол-во
mean_salary <- mean(salary) # Среднее
var_salary <- var(salary) # Дисперсия

# 1.1 Доверительные интервалы для средней зарплаты

# Параметрические доверительные интервалы
se_salary <- sd(salary) / sqrt(n) # Стандартная ошибка
ci_parametric_95_mean <- mean_salary + qt(c(0.025, 0.975), df=n-1) * se_salary
ci_parametric_99_mean <- mean_salary + qt(c(0.005, 0.995), df=n-1) * se_salary

# Бутстрепные доверительные интервалы
ci_bootstrap_95_mean <- boot.ci(boot(salary, function(data, indices) mean(data[indices]), R=1000), type="bca", conf=0.95)
ci_bootstrap_99_mean <- boot.ci(boot(salary, function(data, indices) mean(data[indices]), R=1000), type="bca", conf=0.99)

# 1.2 Доверительные интервалы для дисперсии зарплат

# Параметрические доверительные интервалы
chi_lower_95 <- qchisq(0.975, df=n-1)
chi_upper_95 <- qchisq(0.025, df=n-1)
chi_lower_99 <- qchisq(0.995, df=n-1)
chi_upper_99 <- qchisq(0.005, df=n-1)

ci_parametric_95_var <- c((n-1) * var_salary / chi_lower_95, (n-1) * var_salary / chi_upper_95)
ci_parametric_99_var <- c((n-1) * var_salary / chi_lower_99, (n-1) * var_salary / chi_upper_99)

# Бутстрепные доверительные интервалы
ci_bootstrap_95_var <- boot.ci(boot(salary, function(data, indices) var(data[indices]), R=1000), type="bca", conf=0.95)
ci_bootstrap_99_var <- boot.ci(boot(salary, function(data, indices) var(data[indices]), R=1000), type="bca", conf=0.99)

# Задание 2

# Функция для метода "складного ножа" (jackknife)
jackknife <- function(data, statistic) {
  n <- length(data)
  overall_stat <- statistic(data)
  jackknife_samples <- sapply(1:n, function(i) statistic(data[-i]))
  jack_stat <- n * overall_stat - (n - 1) * mean(jackknife_samples)
  return(jack_stat)
}

# Оценка средней зарплаты и выборочной дисперсии методом "складного ножа"
jackknife_mean_salary <- jackknife(salary, mean)
jackknife_var_salary <- jackknife(salary, var)

# Задание 3

# Загрузка данных
data <- read.table("data/group.txt", header = TRUE)

# Тест Шапиро-Уилка
shapiro_test_height <- shapiro.test(data$Height)
shapiro_test_weight <- shapiro.test(data$Weight)

# Вывод результатов теста Шапиро-Уилка
print(shapiro_test_height)
print(shapiro_test_weight)

# Тест Колмогорова-Смирнова
ks_test_height <- ks.test(data$Height, "pnorm", mean(data$Height), sd(data$Height))
ks_test_weight <- ks.test(data$Weight, "pnorm", mean(data$Weight), sd(data$Weight))

# Вывод результатов теста Колмогорова-Смирнова
print(ks_test_height)
print(ks_test_weight)

# QQ-plot для роста
qqnorm(data$Height, main = "QQ-plot for Height", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(data$Height, col = "red")

# QQ-plot для веса
qqnorm(data$Weight, main = "QQ-plot for Weight", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(data$Weight, col = "red")


# Задание 4

# Функция для расчета параметрических доверительных интервалов
parametric_ci <- function(data, level = 0.95) {
  n <- length(data)
  mean_data <- mean(data)
  var_data <- var(data)
  
  # Доверительные интервалы для среднего
  se_mean <- sd(data) / sqrt(n)
  t_crit <- qt(c((1-level)/2, 1-(1-level)/2), df=n-1)
  ci_mean <- mean_data + t_crit * se_mean
  
  # Доверительные интервалы для дисперсии
  chi_crit <- qchisq(c(1-(1-level)/2, (1-level)/2), df=n-1)
  ci_var <- ((n - 1) * var_data) / chi_crit
  
  return(list(ci_mean = ci_mean, ci_var = ci_var))
}

# Функция для расчета бутстрепных доверительных интервалов
bootstrap_ci <- function(data, level = 0.95) {
  ci_mean <- boot.ci(boot(data, function(data, indices) mean(data[indices]), R=1000), type="bca", conf=level)
  ci_var <- boot.ci(boot(data, function(data, indices) var(data[indices]), R=1000), type="bca", conf=level)
  
  return(list(ci_mean = ci_mean, ci_var = ci_var))
}

# Расчет параметрических и бутстрепных доверительных интервалов для роста и веса
height_parametric_95 <- parametric_ci(data$Height)
weight_parametric_95 <- parametric_ci(data$Weight)
height_bootstrap_95 <- bootstrap_ci(data$Height)
weight_bootstrap_95 <- bootstrap_ci(data$Weight)

height_parametric_99 <- parametric_ci(data$Height, level = 0.99)
weight_parametric_99 <- parametric_ci(data$Weight, level = 0.99)
height_bootstrap_99 <- bootstrap_ci(data$Height, level = 0.99)
weight_bootstrap_99 <- bootstrap_ci(data$Weight, level = 0.99)

# Вывод результатов
print(height_parametric_95)
print(weight_parametric_95)
print(height_bootstrap_95)
print(weight_bootstrap_95)

print(height_parametric_99)
print(weight_parametric_99)
print(height_bootstrap_99)
print(weight_bootstrap_99)


sink()
