sink("Дорошенко.out")

# 1.	Произведите «простые» арифметические операции

# a. Сложение двух матриц размерности 3х3
matrix1 <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3)
matrix2 <- matrix(c(9,8,7,6,5,4,3,2,1), nrow = 3, ncol = 3)
sum_matrix <- matrix1 + matrix2
print(sum_matrix)

# b. Умножение результата сложения на вектор (0,1)

vector <- c(0,1)
product_result <- sum_matrix
product_result[, 1] <- sum_matrix[, 1] * vector[1]
product_result[, 2] <- sum_matrix[, 2] * vector[2]

# c. Возведение в квадрат результата предыдущего пункта
square_result <- product_result^2
print(square_result)

# 2.	Создайте квадратную матрицу А

A <- matrix(c(4,3,2,1), nrow = 2, ncol = 2)

# a. Найдите матрицу A^T * A
ATA <- t(A) %*% A
print(ATA)

# b. Определитель и обратную матрицу для результата предыдущего пункта

det_ATA <- det(ATA)
inv_ATA <- solve(ATA)
print(det_ATA)
print(inv_ATA)

# c. Решите систему уравнений: x+3y-5, 3x+5y=8
coef_matrix <- matrix(c(1, 3, 2, 4), nrow = 2, ncol = 2)
b_vector <- c(5, 8)
solution <- solve(coef_matrix, b_vector)
print(solution)

# 3. Создайте массив размерности 4: 2х3х4х5:

array_4d <- array(1:(2*3*4*5), dim = c(2,3,4,5))


sink();





