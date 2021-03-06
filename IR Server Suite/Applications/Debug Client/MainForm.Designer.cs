namespace DebugClient
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
      this.components = new System.ComponentModel.Container();
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
      this.buttonBlast = new System.Windows.Forms.Button();
      this.buttonLearnIR = new System.Windows.Forms.Button();
      this.labelServerAddress = new System.Windows.Forms.Label();
      this.buttonConnect = new System.Windows.Forms.Button();
      this.buttonDisconnect = new System.Windows.Forms.Button();
      this.buttonShutdownServer = new System.Windows.Forms.Button();
      this.listBoxStatus = new System.Windows.Forms.ListBox();
      this.groupBoxStatus = new System.Windows.Forms.GroupBox();
      this.groupBoxRemoteButton = new System.Windows.Forms.GroupBox();
      this.textBoxRemoteCode = new System.Windows.Forms.TextBox();
      this.textBoxRemoteDevice = new System.Windows.Forms.TextBox();
      this.labelRemoteCode = new System.Windows.Forms.Label();
      this.labelRemoteDevice = new System.Windows.Forms.Label();
      this.buttonSendRemoteButton = new System.Windows.Forms.Button();
      this.groupBoxSetup = new System.Windows.Forms.GroupBox();
      this.comboBoxComputer = new System.Windows.Forms.ComboBox();
      this.groupBoxCommands = new System.Windows.Forms.GroupBox();
      this.comboBoxPort = new System.Windows.Forms.ComboBox();
      this.toolTips = new System.Windows.Forms.ToolTip(this.components);
      this.groupBoxStatus.SuspendLayout();
      this.groupBoxRemoteButton.SuspendLayout();
      this.groupBoxSetup.SuspendLayout();
      this.groupBoxCommands.SuspendLayout();
      this.SuspendLayout();
      // 
      // buttonBlast
      // 
      this.buttonBlast.Location = new System.Drawing.Point(80, 16);
      this.buttonBlast.Name = "buttonBlast";
      this.buttonBlast.Size = new System.Drawing.Size(64, 24);
      this.buttonBlast.TabIndex = 1;
      this.buttonBlast.Text = "Blast IR";
      this.toolTips.SetToolTip(this.buttonBlast, "Blast learned IR Command");
      this.buttonBlast.UseVisualStyleBackColor = true;
      this.buttonBlast.Click += new System.EventHandler(this.buttonBlast_Click);
      // 
      // buttonLearnIR
      // 
      this.buttonLearnIR.Location = new System.Drawing.Point(8, 16);
      this.buttonLearnIR.Name = "buttonLearnIR";
      this.buttonLearnIR.Size = new System.Drawing.Size(64, 24);
      this.buttonLearnIR.TabIndex = 0;
      this.buttonLearnIR.Text = "Learn IR";
      this.toolTips.SetToolTip(this.buttonLearnIR, "Learn an IR Command");
      this.buttonLearnIR.UseVisualStyleBackColor = true;
      this.buttonLearnIR.Click += new System.EventHandler(this.buttonLearnIR_Click);
      // 
      // labelServerAddress
      // 
      this.labelServerAddress.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.labelServerAddress.Location = new System.Drawing.Point(8, 16);
      this.labelServerAddress.Name = "labelServerAddress";
      this.labelServerAddress.Size = new System.Drawing.Size(240, 16);
      this.labelServerAddress.TabIndex = 0;
      this.labelServerAddress.Text = "IR Server host computer:";
      // 
      // buttonConnect
      // 
      this.buttonConnect.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonConnect.Location = new System.Drawing.Point(264, 32);
      this.buttonConnect.Name = "buttonConnect";
      this.buttonConnect.Size = new System.Drawing.Size(80, 24);
      this.buttonConnect.TabIndex = 2;
      this.buttonConnect.Text = "Connect";
      this.toolTips.SetToolTip(this.buttonConnect, "Connect to server");
      this.buttonConnect.UseVisualStyleBackColor = true;
      this.buttonConnect.Click += new System.EventHandler(this.buttonConnect_Click);
      // 
      // buttonDisconnect
      // 
      this.buttonDisconnect.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonDisconnect.Location = new System.Drawing.Point(352, 32);
      this.buttonDisconnect.Name = "buttonDisconnect";
      this.buttonDisconnect.Size = new System.Drawing.Size(80, 24);
      this.buttonDisconnect.TabIndex = 3;
      this.buttonDisconnect.Text = "Disconnect";
      this.toolTips.SetToolTip(this.buttonDisconnect, "Disconnect from server");
      this.buttonDisconnect.UseVisualStyleBackColor = true;
      this.buttonDisconnect.Click += new System.EventHandler(this.buttonDisconnect_Click);
      // 
      // buttonShutdownServer
      // 
      this.buttonShutdownServer.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonShutdownServer.Location = new System.Drawing.Point(368, 16);
      this.buttonShutdownServer.Name = "buttonShutdownServer";
      this.buttonShutdownServer.Size = new System.Drawing.Size(64, 24);
      this.buttonShutdownServer.TabIndex = 4;
      this.buttonShutdownServer.Text = "Shutdown";
      this.toolTips.SetToolTip(this.buttonShutdownServer, "Shutdown the server");
      this.buttonShutdownServer.UseVisualStyleBackColor = true;
      this.buttonShutdownServer.Click += new System.EventHandler(this.buttonShutdownServer_Click);
      // 
      // listBoxStatus
      // 
      this.listBoxStatus.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.listBoxStatus.FormattingEnabled = true;
      this.listBoxStatus.HorizontalScrollbar = true;
      this.listBoxStatus.IntegralHeight = false;
      this.listBoxStatus.Location = new System.Drawing.Point(8, 16);
      this.listBoxStatus.Name = "listBoxStatus";
      this.listBoxStatus.ScrollAlwaysVisible = true;
      this.listBoxStatus.Size = new System.Drawing.Size(424, 211);
      this.listBoxStatus.TabIndex = 0;
      this.toolTips.SetToolTip(this.listBoxStatus, "Status messages");
      // 
      // groupBoxStatus
      // 
      this.groupBoxStatus.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.groupBoxStatus.Controls.Add(this.listBoxStatus);
      this.groupBoxStatus.Location = new System.Drawing.Point(8, 192);
      this.groupBoxStatus.Name = "groupBoxStatus";
      this.groupBoxStatus.Size = new System.Drawing.Size(440, 242);
      this.groupBoxStatus.TabIndex = 3;
      this.groupBoxStatus.TabStop = false;
      this.groupBoxStatus.Text = "Status";
      // 
      // groupBoxRemoteButton
      // 
      this.groupBoxRemoteButton.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.groupBoxRemoteButton.Controls.Add(this.textBoxRemoteCode);
      this.groupBoxRemoteButton.Controls.Add(this.textBoxRemoteDevice);
      this.groupBoxRemoteButton.Controls.Add(this.labelRemoteCode);
      this.groupBoxRemoteButton.Controls.Add(this.labelRemoteDevice);
      this.groupBoxRemoteButton.Controls.Add(this.buttonSendRemoteButton);
      this.groupBoxRemoteButton.Location = new System.Drawing.Point(8, 136);
      this.groupBoxRemoteButton.Name = "groupBoxRemoteButton";
      this.groupBoxRemoteButton.Size = new System.Drawing.Size(440, 48);
      this.groupBoxRemoteButton.TabIndex = 2;
      this.groupBoxRemoteButton.TabStop = false;
      this.groupBoxRemoteButton.Text = "Remote button";
      // 
      // textBoxRemoteCode
      // 
      this.textBoxRemoteCode.Location = new System.Drawing.Point(232, 16);
      this.textBoxRemoteCode.Name = "textBoxRemoteCode";
      this.textBoxRemoteCode.Size = new System.Drawing.Size(100, 20);
      this.textBoxRemoteCode.TabIndex = 7;
      // 
      // textBoxRemoteDevice
      // 
      this.textBoxRemoteDevice.Location = new System.Drawing.Point(64, 16);
      this.textBoxRemoteDevice.Name = "textBoxRemoteDevice";
      this.textBoxRemoteDevice.Size = new System.Drawing.Size(100, 20);
      this.textBoxRemoteDevice.TabIndex = 6;
      // 
      // labelRemoteCode
      // 
      this.labelRemoteCode.Location = new System.Drawing.Point(176, 16);
      this.labelRemoteCode.Name = "labelRemoteCode";
      this.labelRemoteCode.Size = new System.Drawing.Size(56, 20);
      this.labelRemoteCode.TabIndex = 5;
      this.labelRemoteCode.Text = "Code:";
      this.labelRemoteCode.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
      // 
      // labelRemoteDevice
      // 
      this.labelRemoteDevice.Location = new System.Drawing.Point(8, 16);
      this.labelRemoteDevice.Name = "labelRemoteDevice";
      this.labelRemoteDevice.Size = new System.Drawing.Size(56, 20);
      this.labelRemoteDevice.TabIndex = 4;
      this.labelRemoteDevice.Text = "Device:";
      this.labelRemoteDevice.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
      // 
      // buttonSendRemoteButton
      // 
      this.buttonSendRemoteButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonSendRemoteButton.Location = new System.Drawing.Point(376, 16);
      this.buttonSendRemoteButton.Name = "buttonSendRemoteButton";
      this.buttonSendRemoteButton.Size = new System.Drawing.Size(56, 24);
      this.buttonSendRemoteButton.TabIndex = 3;
      this.buttonSendRemoteButton.Text = "Send";
      this.buttonSendRemoteButton.UseVisualStyleBackColor = true;
      this.buttonSendRemoteButton.Click += new System.EventHandler(this.buttonSendRemoteButton_Click);
      // 
      // groupBoxSetup
      // 
      this.groupBoxSetup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.groupBoxSetup.Controls.Add(this.comboBoxComputer);
      this.groupBoxSetup.Controls.Add(this.labelServerAddress);
      this.groupBoxSetup.Controls.Add(this.buttonConnect);
      this.groupBoxSetup.Controls.Add(this.buttonDisconnect);
      this.groupBoxSetup.Location = new System.Drawing.Point(8, 8);
      this.groupBoxSetup.Name = "groupBoxSetup";
      this.groupBoxSetup.Size = new System.Drawing.Size(440, 64);
      this.groupBoxSetup.TabIndex = 0;
      this.groupBoxSetup.TabStop = false;
      this.groupBoxSetup.Text = "Setup";
      // 
      // comboBoxComputer
      // 
      this.comboBoxComputer.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.comboBoxComputer.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.Suggest;
      this.comboBoxComputer.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
      this.comboBoxComputer.FormattingEnabled = true;
      this.comboBoxComputer.Location = new System.Drawing.Point(8, 32);
      this.comboBoxComputer.Name = "comboBoxComputer";
      this.comboBoxComputer.Size = new System.Drawing.Size(240, 21);
      this.comboBoxComputer.TabIndex = 1;
      // 
      // groupBoxCommands
      // 
      this.groupBoxCommands.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.groupBoxCommands.Controls.Add(this.comboBoxPort);
      this.groupBoxCommands.Controls.Add(this.buttonBlast);
      this.groupBoxCommands.Controls.Add(this.buttonLearnIR);
      this.groupBoxCommands.Controls.Add(this.buttonShutdownServer);
      this.groupBoxCommands.Location = new System.Drawing.Point(8, 80);
      this.groupBoxCommands.Name = "groupBoxCommands";
      this.groupBoxCommands.Size = new System.Drawing.Size(440, 48);
      this.groupBoxCommands.TabIndex = 1;
      this.groupBoxCommands.TabStop = false;
      this.groupBoxCommands.Text = "Commands";
      // 
      // comboBoxPort
      // 
      this.comboBoxPort.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
      this.comboBoxPort.FormattingEnabled = true;
      this.comboBoxPort.Location = new System.Drawing.Point(152, 18);
      this.comboBoxPort.Name = "comboBoxPort";
      this.comboBoxPort.Size = new System.Drawing.Size(80, 21);
      this.comboBoxPort.TabIndex = 2;
      this.toolTips.SetToolTip(this.comboBoxPort, "Port for blasting IR");
      // 
      // MainForm
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(464, 446);
      this.Controls.Add(this.groupBoxCommands);
      this.Controls.Add(this.groupBoxSetup);
      this.Controls.Add(this.groupBoxRemoteButton);
      this.Controls.Add(this.groupBoxStatus);
      this.HelpButton = true;
      this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.MinimumSize = new System.Drawing.Size(472, 482);
      this.Name = "MainForm";
      this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
      this.Text = "Debug Client";
      this.Load += new System.EventHandler(this.MainForm_Load);
      this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainForm_FormClosing);
      this.HelpRequested += new System.Windows.Forms.HelpEventHandler(this.MainForm_HelpRequested);
      this.groupBoxStatus.ResumeLayout(false);
      this.groupBoxRemoteButton.ResumeLayout(false);
      this.groupBoxRemoteButton.PerformLayout();
      this.groupBoxSetup.ResumeLayout(false);
      this.groupBoxCommands.ResumeLayout(false);
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.Button buttonBlast;
    private System.Windows.Forms.Button buttonLearnIR;
    private System.Windows.Forms.Label labelServerAddress;
    private System.Windows.Forms.Button buttonConnect;
    private System.Windows.Forms.Button buttonDisconnect;
    private System.Windows.Forms.Button buttonShutdownServer;
    private System.Windows.Forms.ListBox listBoxStatus;
    private System.Windows.Forms.GroupBox groupBoxStatus;
    private System.Windows.Forms.GroupBox groupBoxRemoteButton;
    private System.Windows.Forms.Button buttonSendRemoteButton;
    private System.Windows.Forms.GroupBox groupBoxSetup;
    private System.Windows.Forms.GroupBox groupBoxCommands;
    private System.Windows.Forms.ToolTip toolTips;
    private System.Windows.Forms.ComboBox comboBoxPort;
    private System.Windows.Forms.ComboBox comboBoxComputer;
    private System.Windows.Forms.Label labelRemoteCode;
    private System.Windows.Forms.Label labelRemoteDevice;
    private System.Windows.Forms.TextBox textBoxRemoteCode;
    private System.Windows.Forms.TextBox textBoxRemoteDevice;
  }
}

