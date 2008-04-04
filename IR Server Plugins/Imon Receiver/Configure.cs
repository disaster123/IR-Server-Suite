using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace InputService.Plugin
{

  partial class Configure : Form
  {

    #region Properties

    public bool EnableRemote
    {
      get { return checkBoxEnableRemote.Checked; }
      set { checkBoxEnableRemote.Checked = value; }
    }
    public bool UseSystemRatesForRemote
    {
      get { return checkBoxUseSystemRatesRemote.Checked; }
      set { checkBoxUseSystemRatesRemote.Checked = value; }
    }
    public int RemoteRepeatDelay
    {
      get { return Decimal.ToInt32(numericUpDownButtonRepeatDelay.Value); }
      set { numericUpDownButtonRepeatDelay.Value = new Decimal(value); }
    }
    public int RemoteHeldDelay
    {
      get { return Decimal.ToInt32(numericUpDownButtonHeldDelay.Value); }
      set { numericUpDownButtonHeldDelay.Value = new Decimal(value); }
    }

    public bool EnableKeyboard
    {
      get { return checkBoxEnableKeyboard.Checked; }
      set { checkBoxEnableKeyboard.Checked = value; }
    }
    public bool UseSystemRatesForKeyboard
    {
      get { return checkBoxUseSystemRatesKeyboard.Checked; }
      set { checkBoxUseSystemRatesKeyboard.Checked = value; }
    }
    public int KeyboardRepeatDelay
    {
      get { return Decimal.ToInt32(numericUpDownKeyRepeatDelay.Value); }
      set { numericUpDownKeyRepeatDelay.Value = new Decimal(value); }
    }
    public int KeyboardHeldDelay
    {
      get { return Decimal.ToInt32(numericUpDownKeyHeldDelay.Value); }
      set { numericUpDownKeyHeldDelay.Value = new Decimal(value); }
    }
    public bool HandleKeyboardLocal
    {
      get { return checkBoxHandleKeyboardLocal.Checked; }
      set { checkBoxHandleKeyboardLocal.Checked = value; }
    }

    public bool EnableMouse
    {
      get { return checkBoxEnableMouse.Checked; }
      set { checkBoxEnableMouse.Checked = value; }
    }
    public double MouseSensitivity
    {
      get { return Decimal.ToDouble(numericUpDownMouseSensitivity.Value); }
      set { numericUpDownMouseSensitivity.Value = new Decimal(value); }
    }
    public bool HandleMouseLocal
    {
      get { return checkBoxHandleMouseLocal.Checked; }
      set { checkBoxHandleMouseLocal.Checked = value; }
    }

    #endregion Properties

    #region Constructor

    /// <summary>
    /// Initializes a new instance of the <see cref="Configure"/> class.
    /// </summary>
    public Configure()
    {
      InitializeComponent();
    }

    #endregion Constructor

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

    #endregion Buttons

    private void checkBoxUseSystemRatesRemote_CheckedChanged(object sender, EventArgs e)
    {
      groupBoxRemoteTiming.Enabled = !checkBoxUseSystemRatesRemote.Checked;
    }
    private void checkBoxUseSystemRatesKeyboard_CheckedChanged(object sender, EventArgs e)
    {
      groupBoxKeypressTiming.Enabled = !checkBoxUseSystemRatesKeyboard.Checked;
    }

  }

}
