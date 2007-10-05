using System;

namespace IRServerPluginInterface
{

  #region Delegates

  /// <summary>
  /// IR Server callback for remote button press handling.
  /// </summary>
  /// <param name="keyCode">Remote button code.</param>
  public delegate void RemoteHandler(string keyCode);

  #endregion Delegates
  
  /// <summary>
  /// Plugins that implement this interface can receive remote control button presses.
  /// </summary>
  public interface IRemoteReceiver
  {

    /// <summary>
    /// Callback for remote button presses.
    /// </summary>
    RemoteHandler RemoteCallback { get; set; }

  }

}