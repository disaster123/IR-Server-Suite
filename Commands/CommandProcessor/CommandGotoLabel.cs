using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace Commands
{

  /// <summary>
  /// Goto Label macro command.
  /// </summary>
  public class CommandGotoLabel : Command
  {

    #region Constructors

    /// <summary>
    /// Initializes a new instance of the <see cref="CommandGotoLabel"/> class.
    /// </summary>
    public CommandGotoLabel() { InitParameters(1); }
    
    /// <summary>
    /// Initializes a new instance of the <see cref="CommandGotoLabel"/> class.
    /// </summary>
    /// <param name="parameters">The parameters.</param>
    public CommandGotoLabel(string[] parameters) : base(parameters) { }
    
    #endregion Constructors

    #region Implementation

    /// <summary>
    /// Gets the category of this command.
    /// </summary>
    /// <returns>The category of this command.</returns>
    public override string GetCategory() { return Processor.CategoryMacro; }

    /// <summary>
    /// Gets the user interface text.
    /// </summary>
    /// <returns>User interface text.</returns>
    public override string GetUserInterfaceText() { return "Goto Label"; }

    /// <summary>
    /// Edit this command.
    /// </summary>
    /// <param name="parent">The parent window.</param>
    /// <returns><c>true</c> if the command was modified; otherwise <c>false</c>.</returns>
    public override bool Edit(IWin32Window parent)
    {
      EditLabel edit = new EditLabel(_parameters[0]);
      if (edit.ShowDialog(parent) == DialogResult.OK)
      {
        _parameters[0] = edit.LabelName;
        return true;
      }

      return false;
    }

    #endregion Implementation

  }

}
