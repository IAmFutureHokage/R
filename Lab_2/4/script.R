if (!dir.exists("out")) dir.create("out")
sink("Дорошенко.out")

# Функция для сохранения текущего графика
save_plot <- function(plot_name) {
  dev.copy(png, filename = paste0("out/", plot_name, ".png"))
  dev.off()
}

# 1a. Ступеньки из точек
x <- 1:10
y <- x^2
plot(x, y, type = "s", main = "Ступеньки из точек")
save_plot("staircase")

# 1b. Автомобили 20-х годов
plot(mtcars$wt, mtcars$mpg, main = "Автомобили 20-х годов", xlab = "Вес", ylab = "MPG")
save_plot("cars_20s")

# 1c. Trees
plot(trees$Girth, trees$Volume, main = "Trees", xlab = "Girth", ylab = "Volume")
save_plot("trees")

# 1d. tg(x), -1<=x<=1
x <- seq(-1, 1, by = 0.01)
y <- tan(x)
plot(x, y, main = "tg(x), -1<=x<=1", xlab = "x", ylab = "tg(x)")
save_plot("tan")

# 1e. cos(x), -2π<=x<= 2π
x <- seq(-2*pi, 2*pi, by = 0.01)
y <- cos(x)
plot(x, y, main = "cos(x), -2π<=x<= 2π", xlab = "x", ylab = "cos(x)")
save_plot("cos")

# Загрузка данных и создание графика из файла "example of fine plot.txt"
t = seq(0, 10, by = 0.1) 
y1 = 1.03*exp(-0.25*t)*sin(0.97*t) 
y2 = 1.07*exp(-0.35*t)*sin(0.94*t) 
y3 = 1.15*exp(-0.5*t)*sin(0.87*t) 
y4 = 0.45*(exp(-0.38*t) - exp(-2.62*t)) 
y5 = 0.22*(exp(-0.21*t) - exp(-4.80*t)) 
plot(t, y1, type = "n", main = "Oscillation") 
colors = c("blue", "red", "green", "cyan", "magenta") 
lines(t, y1, col = colors[1]) 
lines(t, y2, col = colors[2]) 
lines(t, y3, col = colors[3]) 
lines(t, y4, col = colors[4]) 
lines(t, y5, col = colors[5]) 
legends = paste("y", 1:5, sep = "") # Вектор legends=("y1", "y2", "y3", "y4", "y5")
legend("topright", legends, lty = "solid", col = colors)  # Легенда перемещена в правый верхний угол
abline(h = 0) # Горизонтальная ось 
abline(v = 0) # Вертикальная ось
save_plot("fine_plot")

# 3. Изменить оформление графика cos(x), -2π<=x<= 2π
plot(x, y, type = "o", col = "blue", main = "cos(x), -2π<=x<= 2π", xlab = "x", ylab = "cos(x)")
abline(h = 0, v = 0, col = "grey")
legend("topright", legend = "cos(x)", col = "blue", lty = 1, pch = 1)
save_plot("cos_modified")

sink();





