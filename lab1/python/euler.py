"""Задачи Эйлера 6 и 15 обычными циклами — для сравнения с Haskell."""


def sum_square_difference(n: int) -> int:
    total = 0
    squares = 0
    for i in range(1, n + 1):
        total += i
        squares += i * i
    return total * total - squares


def lattice_paths(n: int) -> int:
    # row[x] — число маршрутов до узла x в текущей строке узлов.
    # В первой строке все единицы: вдоль края путь один.
    # В узел приходят сверху или слева: row[x] (сверху) += row[x - 1] (слева).
    row = [1] * (n + 1)
    for _ in range(n):
        for x in range(1, n + 1):
            row[x] += row[x - 1]
    return row[n]


if __name__ == "__main__":
    print(sum_square_difference(100))
    print(lattice_paths(20))
