namespace Commands
{

  partial class EditBlastIR
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
      this.components = new System.ComponentModel.Container();
      this.labelIRFile = new System.Windows.Forms.Label();
      this.labelBlasterPort = new System.Windows.Forms.Label();
      this.comboBoxPort = new System.Windows.Forms.ComboBox();
      this.labelIRCommandFile = new System.Windows.Forms.Label();
      this.buttonTest = new System.Windows.Forms.Button();
      this.buttonOK = new System.Windows.Forms.Button();
      this.buttonCancel = new System.Windows.Forms.Button();
      this.checkBoxUseForAll = new System.Windows.Forms.CheckBox();
      this.toolTips = new System.Windows.Forms.ToolTip(this.components);
      this.SuspendLayout();
      // 
      // labelIRFile
      // 
      this.labelIRFile.Location = new System.Drawing.Point(8, 8);
      this.labelIRFile.Name = "labelIRFile";
      this.labelIRFile.Size = new System.Drawing.Size(88, 20);
      this.labelIRFile.TabIndex = 0;
      this.labelIRFile.Text = "IR Command:";
      this.labelIRFile.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
      // 
      // labelBlasterPort
      // 
      this.labelBlasterPort.Location = new System.Drawing.Point(8, 40);
      this.labelBlasterPort.Name = "labelBlasterPort";
      this.labelBlasterPort.Size = new System.Drawing.Size(88, 21);
      this.labelBlasterPort.TabIndex = 2;
      this.labelBlasterPort.Text = "Blaster port:";
      this.labelBlasterPort.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
      // 
      // comboBoxPort
      // 
      this.comboBoxPort.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.comboBoxPort.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
      this.comboBoxPort.FormattingEnabled = true;
      this.comboBoxPort.Location = new System.Drawing.Point(96, 40);
      this.comboBoxPort.Name = "comboBoxPort";
      this.comboBoxPort.Size = new System.Drawing.Size(160, 21);
      this.comboBoxPort.TabIndex = 3;
      this.toolTips.SetToolTip(this.comboBoxPort, "Choose the blaster port to transmit this command to");
      // 
      // labelIRCommandFile
      // 
      this.labelIRCommandFile.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.labelIRCommandFile.AutoEllipsis = true;
      this.labelIRCommandFile.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
      this.labelIRCommandFile.Location = new System.Drawing.Point(96, 8);
      this.labelIRCommandFile.Name = "labelIRCommandFile";
      this.labelIRCommandFile.Size = new System.Drawing.Size(248, 20);
      this.labelIRCommandFile.TabIndex = 1;
      this.labelIRCommandFile.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
      // 
      // buttonTest
      // 
      this.buttonTest.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonTest.Location = new System.Drawing.Point(8, 72);
      this.buttonTest.Name = "buttonTest";
      this.buttonTest.Size = new System.Drawing.Size(56, 24);
      this.buttonTest.TabIndex = 4;
      this.buttonTest.Text = "Test";
      this.toolTips.SetToolTip(this.buttonTest, "Test this blast command");
      this.buttonTest.UseVisualStyleBackColor = true;
      this.buttonTest.Click += new System.EventHandler(this.buttonTest_Click);
      // 
      // buttonOK
      // 
      this.buttonOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonOK.Location = new System.Drawing.Point(224, 72);
      this.buttonOK.Name = "buttonOK";
      this.buttonOK.Size = new System.Drawing.Size(56, 24);
      this.buttonOK.TabIndex = 6;
      this.buttonOK.Text = "OK";
      this.buttonOK.UseVisualStyleBackColor = true;
      this.buttonOK.Click += new System.EventHandler(this.buttonOK_Click);
      // 
      // buttonCancel
      // 
      this.buttonCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
      this.buttonCancel.Location = new System.Drawing.Point(288, 72);
      this.buttonCancel.Name = "buttonCancel";
      this.buttonCancel.Size = new System.Drawing.Size(56, 24);
      this.buttonCancel.TabIndex = 7;
      this.buttonCancel.Text = "Cancel";
      this.buttonCancel.UseVisualStyleBackColor = true;
      this.buttonCancel.Click += new System.EventHandler(this.buttonCancel_Click);
      // 
      // checkBoxUseForAll
      // 
      this.checkBoxUseForAll.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.checkBoxUseForAll.Location = new System.Drawing.Point(72, 72);
      this.checkBoxUseForAll.Name = "checkBoxUseForAll";
      this.checkBoxUseForAll.Size = new System.Drawing.Size(144, 24);
      this.checkBoxUseForAll.TabIndex = 5;
      this.checkBoxUseForAll.Text = "Use this port for all";
      this.toolTips.SetToolTip(this.checkBoxUseForAll, "Use the selected blaster port for all blast commands in this group");
      this.checkBoxUseForAll.UseVisualStyleBackColor = true;
      this.checkBoxUseForAll.Visible = false;
      // 
      // BlastCommand
      // 
      this.AcceptButton = this.buttonOK;
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.CancelButton = this.buttonCancel;
      this.ClientSize = new System.Drawing.Size(352, 105);
      this.Controls.Add(this.checkBoxUseForAll);
      this.Controls.Add(this.buttonOK);
      this.Controls.Add(this.buttonCancel);
      this.Controls.Add(this.buttonTest);
      this.Controls.Add(this.labelIRCommandFile);
      this.Controls.Add(this.labelBlasterPort);
      this.Controls.Add(this.comboBoxPort);
      this.Controls.Add(this.labelIRFile);
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.MinimumSize = new System.Drawing.Size(360, 132);
      this.Name = "BlastCommand";
      this.ShowIcon = false;
      this.ShowInTaskbar = false;
      this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
      this.Text = "Blast Command";
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.Label labelIRFile;
    private System.Windows.Forms.Label labelBlasterPort;
    private System.Windows.Forms.ComboBox comboBoxPort;
    private System.Windows.Forms.Label labelIRCommandFile;
    private System.Windows.Forms.Button buttonTest;
    private System.Windows.Forms.Button buttonOK;
    private System.Windows.Forms.Button buttonCancel;
    private System.Windows.Forms.CheckBox checkBoxUseForAll;
    private System.Windows.Forms.ToolTip toolTips;
  }

}
