class FibonacciRandomGenerator:
    MAX_NUM_VALUES = 1000000  # Максимальное количество чисел

    def __init__(self, min_value, max_value, num_values):
        self.set_range(min_value, max_value)
        self.set_num_values(num_values)

    def set_range(self, min_value, max_value):
        # Проверяем, что минимальное значение не больше максимального
        if not isinstance(min_value, int) or not isinstance(max_value, int):
            raise ValueError("Ошибка: введите числовые значения для диапазона")
        if min_value > max_value:
            raise ValueError("Ошибка: минимальное значение не может быть больше максимального")
        self.min_value = min_value
        self.max_value = max_value

    def set_num_values(self, num_values):
        # Проверяем, что количество чисел не превышает максимальный лимит
        if not isinstance(num_values, int):
            raise ValueError("Ошибка: введите целое число для количества чисел")
        if num_values <= 0 or num_values > self.MAX_NUM_VALUES:
            raise ValueError(f"Ошибка: превышено допустимое количество чисел (до {self.MAX_NUM_VALUES})")
        self.num_values = num_values

    def generate(self):
        a, b = 0, 1
        numbers = []
        for _ in range(self.num_values):
            a, b = b, a + b
            number = self.min_value + (a % (self.max_value - self.min_value + 1))
            numbers.append(number)
        return numbers

