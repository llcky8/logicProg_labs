using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace lab1
{
    public partial class MainForm : Form
    {
        public MainForm()
        {
            this.BackColor = System.Drawing.Color.LightBlue;
            InitializeComponent();
        }
        private void ProcessButton_Click(object sender, EventArgs e)
        {
            try
            {
                int[] numbers = inputTextBox.Text.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)
                                               .Select(int.Parse)
                                               .ToArray();
                int result = CountEqualPrevious(numbers);

                ResultForm resultForm = new ResultForm(numbers, result);
                resultForm.ShowDialog(this); // Открывает окно поверх основного
            }
            catch (Exception)
            {
                MessageBox.Show("Введите корректные числа через пробел", "Ошибка", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private int CountEqualPrevious(int[] numbers)
        {
            if (numbers.Length < 2) return 0;
            int count = 0;
            for (int i = 1; i < numbers.Length; i++)
            {
                if (numbers[i] == numbers[i - 1])
                    count++;
            }
            return count;
        }
    }
    public class ResultForm : Form
    {
        public ResultForm(int[] numbers, int result)
        {
            this.Text = "Результат";
            this.Size = new System.Drawing.Size(380, 125);
            this.StartPosition = FormStartPosition.CenterParent;
            this.FormBorderStyle = FormBorderStyle.FixedDialog;
            // Создаем текст с введенными числами
            string numbersText = "Введенные числа: " + string.Join(", ", numbers);

            // Создаем метку для отображения введенных чисел
            Label numbersLabel = new Label()
            {
                Text = numbersText,
                AutoSize = true,
                Location = new System.Drawing.Point(10, 20)
            };

            // Создаем метку для отображения результата
            Label resultLabel = new Label()
            {
                Text = "Количество элементов, равных своему предыдущему: " + result,
                AutoSize = true,
                Location = new System.Drawing.Point(10, 50)
            };

            // Добавляем метки на форму
            this.Controls.Add(numbersLabel);
            this.Controls.Add(resultLabel);
        }
    }
}
