namespace Translator
{
  partial class WindowList
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
      this.listBoxWindows = new System.Windows.Forms.ListBox();
      this.SuspendLayout();
      // 
      // listBoxWindows
      // 
      this.listBoxWindows.Dock = System.Windows.Forms.DockStyle.Fill;
      this.listBoxWindows.FormattingEnabled = true;
      this.listBoxWindows.HorizontalScrollbar = true;
      this.listBoxWindows.IntegralHeight = false;
      this.listBoxWindows.Location = new System.Drawing.Point(0, 0);
      this.listBoxWindows.Name = "listBoxWindows";
      this.listBoxWindows.Size = new System.Drawing.Size(292, 376);
      this.listBoxWindows.TabIndex = 0;
      this.listBoxWindows.DoubleClick += new System.EventHandler(this.listBoxWindows_DoubleClick);
      this.listBoxWindows.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.listBoxWindows_KeyPress);
      // 
      // WindowList
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(292, 376);
      this.Controls.Add(this.listBoxWindows);
      this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.Name = "WindowList";
      this.ShowIcon = false;
      this.ShowInTaskbar = false;
      this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
      this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
      this.Text = "Window List";
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.ListBox listBoxWindows;
  }
}