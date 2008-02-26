using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.ServiceProcess;
using System.Text;
using System.Windows.Forms;

using Microsoft.Win32;

namespace InputService.Plugin
{

  partial class Configure : Form
  {

    #region Constants

    const string RegistrySubKey = @"SYSTEM\CurrentControlSet\Services\HidIr\Remotes\745a17a0-74d3-11d0-b6fe-00a0c90f57da";

    #endregion Constants

    #region Properties

    public int LearnTimeout
    {
      get { return Decimal.ToInt32(numericUpDownLearnTimeout.Value); }
      set { numericUpDownLearnTimeout.Value = new Decimal(value); }
    }
    public bool DisableMceServices
    {
      get { return checkBoxDisableMCEServices.Checked; }
      set { checkBoxDisableMCEServices.Checked = value; }
    }

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
    public bool DisableAutomaticButtons
    {
      get { return checkBoxDisableAutomaticButtons.Checked; }
      set { checkBoxDisableAutomaticButtons.Checked = value; }
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

    public Configure()
    {
      InitializeComponent();

      if (Registry.LocalMachine.GetValue(RegistrySubKey + @"\CodeSetNum0", null) == null)
        checkBoxDisableAutomaticButtons.Checked = true;
      else
        checkBoxDisableAutomaticButtons.Checked = false;
    }

    #endregion Constructor

    #region Buttons

    private void buttonOK_Click(object sender, EventArgs e)
    {
      bool changeMade = false;

      using (RegistryKey key = Registry.LocalMachine.OpenSubKey(RegistrySubKey, true))
      {
        bool keysExist = (key.GetValue("CodeSetNum0", null) != null);

        if (checkBoxDisableAutomaticButtons.Checked && keysExist)
        {
          key.DeleteValue("CodeSetNum0", false);
          key.DeleteValue("CodeSetNum1", false);
          key.DeleteValue("CodeSetNum2", false);
          key.DeleteValue("CodeSetNum3", false);

          changeMade = true;
        }
        else if (!checkBoxDisableAutomaticButtons.Checked && !keysExist)
        {
          key.SetValue("CodeSetNum0", 1, RegistryValueKind.DWord);
          key.SetValue("CodeSetNum1", 2, RegistryValueKind.DWord);
          key.SetValue("CodeSetNum2", 3, RegistryValueKind.DWord);
          key.SetValue("CodeSetNum3", 4, RegistryValueKind.DWord);

          changeMade = true;
        }
      }

      if (changeMade)
      {
        RestartService("blah"); // HidServ?

        MessageBox.Show(this, "You must restart for changes to automatic button handling to take effect", "Restart required", MessageBoxButtons.OK, MessageBoxIcon.Information);
      }

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


    static void RestartService(string serviceName)
    {
      try
      {
        ServiceController[] services = ServiceController.GetServices();
        foreach (ServiceController service in services)
        {
          System.Diagnostics.Trace.WriteLine(service.ServiceName);

          if (service.ServiceName.Equals(serviceName, StringComparison.OrdinalIgnoreCase))
          {
            if (service.Status != ServiceControllerStatus.Stopped)
            {
              service.Stop();
              service.WaitForStatus(ServiceControllerStatus.Stopped, new TimeSpan(0, 0, 30));
            }

            service.Start();
          }
        }
      }
      catch (System.ComponentModel.Win32Exception ex)
      {
        MessageBox.Show(ex.Message, "Error restarting service", MessageBoxButtons.OK, MessageBoxIcon.Error);
      }
      catch (System.ServiceProcess.TimeoutException ex)
      {
        MessageBox.Show(ex.Message, "Error stopping service", MessageBoxButtons.OK, MessageBoxIcon.Error);
      }
    }


  }

}
