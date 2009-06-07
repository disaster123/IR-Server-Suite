using System;
using System.Windows.Forms;

namespace Commands
{
  /// <summary>
  /// String Join macro command.
  /// </summary>
  public class CommandStringJoin : Command
  {
    #region Constructors

    /// <summary>
    /// Initializes a new instance of the <see cref="CommandStringJoin"/> class.
    /// </summary>
    public CommandStringJoin()
    {
      InitParameters(3);
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="CommandStringJoin"/> class.
    /// </summary>
    /// <param name="parameters">The parameters.</param>
    public CommandStringJoin(string[] parameters) : base(parameters)
    {
    }

    #endregion Constructors

    #region Implementation

    /// <summary>
    /// Gets the category of this command.
    /// </summary>
    /// <returns>The category of this command.</returns>
    public override string GetCategory()
    {
      return Processor.CategoryString;
    }

    /// <summary>
    /// Gets the user interface text.
    /// </summary>
    /// <returns>User interface text.</returns>
    public override string GetUserInterfaceText()
    {
      return "String Join";
    }

    /// <summary>
    /// Edit this command.
    /// </summary>
    /// <param name="parent">The parent window.</param>
    /// <returns><c>true</c> if the command was modified; otherwise <c>false</c>.</returns>
    public override bool Edit(IWin32Window parent)
    {
      EditStringOperation edit = new EditStringOperation(Parameters);
      if (edit.ShowDialog(parent) == DialogResult.OK)
      {
        Parameters = edit.Parameters;
        return true;
      }

      return false;
    }

    /// <summary>
    /// Execute this command.
    /// </summary>
    /// <param name="variables">The variable list of the calling code.</param>
    public override void Execute(VariableList variables)
    {
      string[] processed = ProcessParameters(variables, Parameters);

      string output = String.Concat(processed[0], processed[1]);

      variables.VariableSet(processed[2], output);
    }

    #endregion Implementation
  }
}