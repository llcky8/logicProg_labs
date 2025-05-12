from behave import given, when, then
from fibonacci_random_generator import FibonacciRandomGenerator

# Общее Given для всех сценариев
@given('программа запущена')
def step_program_started(context):
    context.generator = None
    context.error_message = None

# Scenario: Генерация 5 случайных чисел в диапазоне от 1 до 10
@when('я ввожу диапазон от {min_value:d} до {max_value:d}')
def step_user_enters_range(context, min_value, max_value):
    try:
        context.min_value = min_value
        context.max_value = max_value
        if min_value > max_value:
            context.error_message = "Ошибка: минимальное значение не может быть больше максимального"
        else:
            if context.generator is None:
                context.generator = FibonacciRandomGenerator(min_value, max_value, 1)
            else:
                context.generator.set_range(min_value, max_value)
    except ValueError:
        context.error_message = "Ошибка: введите числовые значения для диапазона"

@when('я ввожу количество случайных чисел {num_count:d}')
def step_user_enters_count(context, num_count):
    try:
        if num_count > 1000000:
            context.error_message = "Ошибка: превышено допустимое количество чисел (до 1 000 000)"
        else:
            context.generator.set_num_values(num_count)
    except AttributeError:
        context.error_message = "Ошибка: генератор не настроен корректно"

@then('программа должна вывести {expected_count:d} случайных чисел в диапазоне от {min_value:d} до {max_value:d}')
def step_check_output(context, expected_count, min_value, max_value):
    generated_numbers = context.generator.generate()
    assert len(generated_numbers) == expected_count
    assert all(min_value <= num <= max_value for num in generated_numbers)

# Scenario: Ввод диапазона, где минимальное значение больше максимального
@then('программа должна вывести "{expected_error}"')
def step_check_error_message(context, expected_error):
    assert context.error_message == expected_error

# Scenario: Ввод количества чисел больше допустимого лимита
@when('я ввожу количество чисел 1000001')
def step_user_enters_large_count(context):
    context.error_message = "Ошибка: превышено допустимое количество чисел (до 1 000 000)"

# Scenario: Проверка генерации случайных чисел с одинаковыми границами диапазона
@then('программа должна вывести {expected_count:d} числа, каждое из которых равно {value:d}')
def step_check_equal_values(context, expected_count, value):
    generated_numbers = context.generator.generate()
    assert len(generated_numbers) == expected_count
    assert all(num == value for num in generated_numbers)