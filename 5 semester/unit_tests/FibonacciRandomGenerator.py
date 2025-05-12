class FibonacciRandomGenerator:
    def __init__(self, min_value, max_value, count):
        if not isinstance(min_value, (int, float)) or not isinstance(max_value, (int, float)):
            raise ValueError("Ошибка: введите числовые значения для диапазона")
        if not isinstance(count, int):
            raise ValueError("Ошибка: введите числовое значение для количества чисел")
        if count <= 0:
            raise ValueError("Ошибка: количество чисел должно быть положительным")
        if count > 1000000:
            raise ValueError("Ошибка: превышено допустимое количество чисел (до 1 000 000)")
        if min_value > max_value:
            raise ValueError("Ошибка: минимальное значение не может быть больше максимального")

        self.min_value = min_value
        self.max_value = max_value
        self.count = count

    def generate(self):
        # Генерация случайных чисел на основе последовательности Фибоначчи
        fib_numbers = self.fibonacci(self.count + 2)  # +2 для получения нужного количества
        random_numbers = [
            (fib_numbers[i] % (self.max_value - self.min_value + 1)) + self.min_value 
            for i in range(2, self.count + 2)
        ]
        return random_numbers

    @staticmethod
    def fibonacci(n):
        a, b = 0, 1
        fib_sequence = []
        for _ in range(n):
            fib_sequence.append(a)
            a, b = b, a + b
        return fib_sequence
