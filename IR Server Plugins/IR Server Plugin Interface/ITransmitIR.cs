using System;

namespace InputService.Plugin
{

  /// <summary>
  /// Plugins that implement this interface can transmit IR commands.
  /// </summary>
  public interface ITransmitIR
  {

    /// <summary>
    /// Lists the available blaster ports.
    /// </summary>
    /// <value>The available ports.</value>
    string[] AvailablePorts { get; }

    /// <summary>
    /// Transmit an infrared command.
    /// </summary>
    /// <param name="port">Port to transmit on.</param>
    /// <param name="data">Data to transmit.</param>
    /// <returns><c>true</c> if successful, otherwise <c>false</c>.</returns>
    bool Transmit(string port, byte[] data);

  }

}
