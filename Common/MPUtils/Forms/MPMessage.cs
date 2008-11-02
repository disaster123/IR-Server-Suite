using System;
using System.Windows.Forms;
using MediaPortal.GUI.Library;

namespace MPUtils.Forms
{
  /// <summary>
  /// Send MediaPortal Message command form.
  /// </summary>
  public partial class MPMessage : Form
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
        return String.Format("{0}|{1}|{2}|{3}|{4}|{5}",
                             comboBoxMessageType.Text,
                             textBoxWindowId.Text.Trim(),
                             textBoxSenderId.Text.Trim(),
                             textBoxControlId.Text.Trim(),
                             textBoxParam1.Text.Trim(),
                             textBoxParam2.Text.Trim());
      }
    }

    #endregion Properties

    #region Constructors

    /// <summary>
    /// Default Constructor.
    /// </summary>
    public MPMessage()
    {
      InitializeComponent();

      SetupComboBox();
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="MPMessage"/> class.
    /// </summary>
    /// <param name="parameters">The parameters.</param>
    public MPMessage(string[] parameters)
      : this()
    {
      comboBoxMessageType.SelectedItem = parameters[0];
      textBoxWindowId.Text = parameters[1];
      textBoxSenderId.Text = parameters[2];
      textBoxControlId.Text = parameters[3];
      textBoxParam1.Text = parameters[4];
      textBoxParam2.Text = parameters[5];
    }

    #endregion Constructors

    private void SetupComboBox()
    {
      comboBoxMessageType.Items.Clear();

      string[] items = Enum.GetNames(typeof (GUIMessage.MessageType));
      Array.Sort(items);

      comboBoxMessageType.Items.AddRange(items);
    }

    private void buttonOK_Click(object sender, EventArgs e)
    {
      DialogResult = DialogResult.OK;
      Close();
    }

    private void buttonCancel_Click(object sender, EventArgs e)
    {
      DialogResult = DialogResult.Cancel;
      Close();
    }
  }
}