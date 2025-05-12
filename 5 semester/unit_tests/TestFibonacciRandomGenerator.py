import unittest
from FibonacciRandomGenerator import FibonacciRandomGenerator


class TestFibonacciRandomGenerator(unittest.TestCase):

    def test_floating_point_range(self):
        min_value = 1.1
        max_value = 10.5
        count = 5
        print(f"Тест: Генерация чисел с плавающей запятой. Диапазон: [{min_value}, {max_value}], Количество: {count}")
        generator = FibonacciRandomGenerator(min_value, max_value, count)
        random_numbers = generator.generate()
        print(f"Сгенерированные числа: {random_numbers}")
        self.assertTrue(all(min_value <= num <= max_value for num in random_numbers))

    def test_generation_with_equal_bounds(self):
        min_value = 5
        max_value = 5
        count = 3
        print(f"Тест: Генерация чисел при равных границах. Значение: {min_value}, Количество: {count}")
        generator = FibonacciRandomGenerator(min_value, max_value, count)
        random_numbers = generator.generate()
        print(f"Сгенерированные числа: {random_numbers}")
        self.assertTrue(all(num == min_value for num in random_numbers))
        self.assertEqual(len(random_numbers), count)

    def test_invalid_input_range(self):
        print("Тест: Нечисловое значение в диапазоне")
        with self.assertRaises(ValueError) as context:
            FibonacciRandomGenerator("abc", 10, 5)
        print(f"Ожидаемое исключение: {str(context.exception)}")
        self.assertEqual(str(context.exception), "Ошибка: введите числовые значения для диапазона")

    def test_large_number_generation(self):
        print("Тест: Количество чисел больше допустимого")
        with self.assertRaises(ValueError) as context:
            FibonacciRandomGenerator(1, 10, 1000001)
        print(f"Ожидаемое исключение: {str(context.exception)}")
        self.assertEqual(str(context.exception), "Ошибка: превышено допустимое количество чисел (до 1 000 000)")

    def test_min_greater_than_max(self):
        print("Тест: Минимальное значение больше максимального")
        with self.assertRaises(ValueError) as context:
            FibonacciRandomGenerator(10, 5, 5)
        print(f"Ожидаемое исключение: {str(context.exception)}")
        self.assertEqual(str(context.exception), "Ошибка: минимальное значение не может быть больше максимального")

    def test_negative_range(self):
        min_value = -10
        max_value = -1
        count = 5
        print(f"Тест: Генерация чисел в отрицательном диапазоне. Диапазон: [{min_value}, {max_value}], Количество: {count}")
        generator = FibonacciRandomGenerator(min_value, max_value, count)
        random_numbers = generator.generate()
        print(f"Сгенерированные числа: {random_numbers}")
        self.assertTrue(all(min_value <= num <= max_value for num in random_numbers))

    def test_non_numeric_count(self):
        print("Тест: Нечисловое значение для количества чисел")
        with self.assertRaises(ValueError) as context:
            FibonacciRandomGenerator(1, 10, "abc")
        print(f"Ожидаемое исключение: {str(context.exception)}")
        self.assertEqual(str(context.exception), "Ошибка: введите числовое значение для количества чисел")

    def test_successful_generation(self):
        min_value = 1
        max_value = 10
        count = 5
        print(f"Тест: Генерация чисел. Диапазон: [{min_value}, {max_value}], Количество: {count}")
        generator = FibonacciRandomGenerator(min_value, max_value, count)
        random_numbers = generator.generate()
        print(f"Сгенерированные числа: {random_numbers}")
        self.assertEqual(len(random_numbers), count)
        for num in random_numbers:
            self.assertGreaterEqual(num, min_value)
            self.assertLessEqual(num, max_value)

if __name__ == '__main__':
    unittest.main()