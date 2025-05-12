namespace lab1
{
    partial class MainForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            processButton = new Button();
            inputTextBox = new TextBox();
            label = new Label();
            SuspendLayout();
            // 
            // processButton
            // 
            processButton.Anchor = AnchorStyles.None;
            processButton.Font = new Font("Microsoft Sans Serif", 14F);
            processButton.Location = new Point(178, 259);
            processButton.Name = "processButton";
            processButton.Size = new Size(236, 54);
            processButton.TabIndex = 0;
            processButton.Text = "Посчитать";
            processButton.UseVisualStyleBackColor = true;
            processButton.Click += ProcessButton_Click;
            // 
            // inputTextBox
            // 
            inputTextBox.Font = new Font("Microsoft Sans Serif", 14F);
            inputTextBox.Location = new Point(178, 164);
            inputTextBox.Name = "inputTextBox";
            inputTextBox.Size = new Size(236, 34);
            inputTextBox.TabIndex = 1;
            inputTextBox.TextAlign = HorizontalAlignment.Center;
            // 
            // label
            // 
            label.AutoSize = true;
            label.Font = new Font("Microsoft Sans Serif", 14F);
            label.Location = new Point(91, 68);
            label.Name = "label";
            label.Size = new Size(446, 29);
            label.TabIndex = 2;
            label.Text = "Введите список чисел через пробел:";
            // 
            // MainForm
            // 
            AutoScaleDimensions = new SizeF(8F, 20F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(600, 390);
            Controls.Add(label);
            Controls.Add(inputTextBox);
            Controls.Add(processButton);
            Name = "MainForm";
            Text = "Счетчик повторяющихся элементов";
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion

        private Button processButton;
        private TextBox inputTextBox;
        private Label label;
    }
}