using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace IrssUtils.Forms
{

  public partial class BlastCommand : Form
  {

    #region Properties

    public string CommandString
    {
      get
      {
        return String.Format("{0}|{1}",
          labelIRCommandFile.Text,
          comboBoxPort.SelectedItem as string);
      }
    }

    public string BlasterPort
    {
      get { return comboBoxPort.SelectedItem as string; }
      set { comboBoxPort.SelectedItem = value; }
    }

    public bool UseForAll
    {
      get { return checkBoxUseForAll.Checked; }
    }

    #endregion Properties

    #region Variables

    string _baseFolder;

    BlastIrDelegate _blastIrDelegate;

    #endregion Variables

    #region Constructors

    BlastCommand(BlastIrDelegate blastIrDelegate, string baseFolder, string[] ports)
    {
      if (blastIrDelegate == null)
        throw new ArgumentNullException("blastIrDelegate");

      if (String.IsNullOrEmpty(baseFolder))
        throw new ArgumentNullException("baseFolder");

      if (ports == null)
        throw new ArgumentNullException("ports");

      InitializeComponent();

      _blastIrDelegate = blastIrDelegate;

      _baseFolder = baseFolder;

      comboBoxPort.Items.AddRange(ports);
      comboBoxPort.SelectedIndex = 0;
    }

    public BlastCommand(BlastIrDelegate blastIrDelegate, string baseFolder, string[] ports, string name)
      : this(blastIrDelegate, baseFolder, ports)
    {
      if (String.IsNullOrEmpty(name))
        throw new ArgumentNullException("fileName");

      labelIRCommandFile.Text = name;
    }

    public BlastCommand(BlastIrDelegate blastIrDelegate, string baseFolder, string[] ports, string[] commands)
      : this(blastIrDelegate, baseFolder, ports)
    {
      if (commands == null)
        throw new ArgumentNullException("commands");

      labelIRCommandFile.Text = commands[0];

      if (comboBoxPort.Items.Contains(commands[1]))
        comboBoxPort.SelectedItem = commands[1];
    }

    public BlastCommand(BlastIrDelegate blastIrDelegate, string baseFolder, string[] ports, string name, bool useAllCheckBoxVisible, int commandCount)
      : this(blastIrDelegate, baseFolder, ports, name)
    {
      checkBoxUseForAll.Text = String.Format("Use this port for all ({0})", commandCount);
      checkBoxUseForAll.Visible = true;
    }

    public BlastCommand(BlastIrDelegate blastIrDelegate, string baseFolder, string[] ports, string[] commands, bool useAllCheckBoxVisible, int commandCount)
      : this(blastIrDelegate, baseFolder, ports, commands)
    {
      checkBoxUseForAll.Text = String.Format("Use this port for all ({0})", commandCount);
      checkBoxUseForAll.Visible = true;
    }

    #endregion Constructors

    #region Buttons

    private void buttonOK_Click(object sender, EventArgs e)
    {
      this.DialogResult = DialogResult.OK;
      this.Close();
    }

    private void buttonCancel_Click(object sender, EventArgs e)
    {
      this.DialogResult = DialogResult.Cancel;
      this.Close();
    }

    private void buttonTest_Click(object sender, EventArgs e)
    {
      string name = labelIRCommandFile.Text.Trim();

      if (name.Length == 0)
        return;

      try
      {
        string fileName = String.Format("{0}{1}{2}", _baseFolder, name, Common.FileExtensionIR);
        string port = comboBoxPort.SelectedItem as string;

        _blastIrDelegate(fileName, port);
      }
      catch (Exception ex)
      {
        MessageBox.Show(this, ex.Message, "Test failed", MessageBoxButtons.OK, MessageBoxIcon.Error);
      }
    }

    #endregion Buttons

  }

}