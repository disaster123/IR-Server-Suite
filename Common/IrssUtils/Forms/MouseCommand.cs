using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace IrssUtils.Forms
{

  /// <summary>
  /// Mouse Command form.
  /// </summary>
  public partial class MouseCommand : Form
  {

    #region Properties

    /// <summary>
    /// Gets the command string.
    /// </summary>
    /// <value>The command string.</value>
    public string CommandString
    {
      get
      {
        StringBuilder command = new StringBuilder();

        if (checkBoxMouseClickLeft.Checked)         command.Append(Common.MouseClickLeft);
        else if (checkBoxMouseClickMiddle.Checked)  command.Append(Common.MouseClickMiddle);
        else if (checkBoxMouseClickRight.Checked)   command.Append(Common.MouseClickRight);
        else if (checkBoxMouseDoubleLeft.Checked)   command.Append(Common.MouseDoubleClickLeft);
        else if (checkBoxMouseDoubleMiddle.Checked) command.Append(Common.MouseDoubleClickMiddle);
        else if (checkBoxMouseDoubleRight.Checked)  command.Append(Common.MouseDoubleClickRight);
        else if (checkBoxMouseScrollUp.Checked)     command.Append(Common.MouseScrollUp);
        else if (checkBoxMouseScrollDown.Checked)   command.Append(Common.MouseScrollDown);
        else if (checkBoxMouseMoveToPos.Checked)
        {
          command.Append(Common.MouseMoveToPos);
          command.Append(numericUpDownX.Value);
          command.Append(',');
          command.Append(numericUpDownY.Value);
        }
        else
        {
          if (checkBoxMouseMoveUp.Checked)          command.Append(Common.MouseMoveUp);
          else if (checkBoxMouseMoveDown.Checked)   command.Append(Common.MouseMoveDown);
          else if (checkBoxMouseMoveLeft.Checked)   command.Append(Common.MouseMoveLeft);
          else if (checkBoxMouseMoveRight.Checked)  command.Append(Common.MouseMoveRight);
          else
            return "None";

          command.Append(numericUpDownMouseMove.Value.ToString());
        }

        return command.ToString();
      }
    }

    #endregion Properties

    #region Constructors

    /// <summary>
    /// Initializes a new instance of the <see cref="MouseCommand"/> class.
    /// </summary>
    public MouseCommand()
    {
      InitializeComponent();
    }
    /// <summary>
    /// Initializes a new instance of the <see cref="MouseCommand"/> class.
    /// </summary>
    /// <param name="command">The command.</param>
    public MouseCommand(string command)
      : this()
    {
      if (String.IsNullOrEmpty(command))
        throw new ArgumentNullException("command");

      switch (command)
      {
        case Common.MouseClickLeft:         checkBoxMouseClickLeft.Checked    = true;   break;
        case Common.MouseClickMiddle:       checkBoxMouseClickMiddle.Checked  = true;   break;
        case Common.MouseClickRight:        checkBoxMouseClickRight.Checked   = true;   break;
        case Common.MouseDoubleClickLeft:   checkBoxMouseDoubleLeft.Checked   = true;   break;
        case Common.MouseDoubleClickMiddle: checkBoxMouseDoubleMiddle.Checked = true;   break;
        case Common.MouseDoubleClickRight:  checkBoxMouseDoubleRight.Checked  = true;   break;
        case Common.MouseScrollDown:        checkBoxMouseScrollDown.Checked   = true;   break;
        case Common.MouseScrollUp:          checkBoxMouseScrollUp.Checked     = true;   break;

        default:
          if (command.StartsWith(Common.MouseMoveDown, StringComparison.OrdinalIgnoreCase))
            checkBoxMouseMoveDown.Checked   = true;
          else if (command.StartsWith(Common.MouseMoveLeft, StringComparison.OrdinalIgnoreCase))
            checkBoxMouseMoveLeft.Checked   = true;
          else if (command.StartsWith(Common.MouseMoveRight, StringComparison.OrdinalIgnoreCase))
            checkBoxMouseMoveRight.Checked  = true;
          else if (command.StartsWith(Common.MouseMoveUp, StringComparison.OrdinalIgnoreCase))
            checkBoxMouseMoveUp.Checked     = true;
          else if (command.StartsWith(Common.MouseMoveToPos, StringComparison.OrdinalIgnoreCase))
          {
            checkBoxMouseMoveToPos.Checked  = true;

            string subString = command.Substring(Common.MouseMoveToPos.Length);

            string[] coords = subString.Split(',');

            numericUpDownX.Value = Decimal.Parse(coords[0]);
            numericUpDownY.Value = Decimal.Parse(coords[1]);
            break;
          }
          else
            break;

          numericUpDownMouseMove.Value = Decimal.Parse(command.Substring(command.IndexOf(' ')));
          break;
      }
  }

    #endregion

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

    private void checkBoxMouse_CheckedChanged(object sender, EventArgs e)
    {
      CheckBox origin = (CheckBox)sender;

      if (!origin.Checked)
        return;

      if (origin != checkBoxMouseMoveToPos)     checkBoxMouseMoveToPos.Checked    = false;
      if (origin != checkBoxMouseClickLeft)     checkBoxMouseClickLeft.Checked    = false;
      if (origin != checkBoxMouseClickRight)    checkBoxMouseClickRight.Checked   = false;
      if (origin != checkBoxMouseClickMiddle)   checkBoxMouseClickMiddle.Checked  = false;
      if (origin != checkBoxMouseDoubleLeft)    checkBoxMouseDoubleLeft.Checked   = false;
      if (origin != checkBoxMouseDoubleRight)   checkBoxMouseDoubleRight.Checked  = false;
      if (origin != checkBoxMouseDoubleMiddle)  checkBoxMouseDoubleMiddle.Checked = false;
      if (origin != checkBoxMouseMoveUp)        checkBoxMouseMoveUp.Checked       = false;
      if (origin != checkBoxMouseMoveDown)      checkBoxMouseMoveDown.Checked     = false;
      if (origin != checkBoxMouseMoveLeft)      checkBoxMouseMoveLeft.Checked     = false;
      if (origin != checkBoxMouseMoveRight)     checkBoxMouseMoveRight.Checked    = false;
      if (origin != checkBoxMouseScrollUp)      checkBoxMouseScrollUp.Checked     = false;
      if (origin != checkBoxMouseScrollDown)    checkBoxMouseScrollDown.Checked   = false;
    }

  }

}
