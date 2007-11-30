using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Ports;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.Xml;

using IRServerPluginInterface;

namespace RedEyeBlaster
{

  /// <summary>
  /// IR Server Plugin for RedEye serial IR Blaster device.  http://www.redremote.co.uk/serial/
  /// </summary>
  public class SerialIRBlaster : IRServerPluginBase, IConfigure, ITransmitIR
  {

    #region Enumerations

    /// <summary>
    /// Used to determine the blaster mode.
    /// </summary>
    enum BlastMode
    {
      /// <summary>
      /// Use the IRDA protocol.
      /// </summary>
      IRDA,
      /// <summary>
      /// Use the RC5 protocol.
      /// </summary>
      RC5,
      /// <summary>
      /// Use the Sky STB protocol.
      /// </summary>
      Sky,
    }

    #endregion Enumerations

    #region Constants

    static readonly string ConfigurationFile =
      Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData) +
      "\\IR Server Suite\\IR Server\\RedEye Blaster.xml";

    static readonly string[] Ports = new string[] { "Default" };

    static const string BlastModeIRDA = "%";
    static const string BlastModeRC5  = "&";
    static const string BlastModeSky  = "$";

    #endregion Constants

    #region Variables

    SerialPort _serialPort;

    string _serialPortName;
    BlastMode _blastMode;

    bool _disposed = false;

    #endregion Variables

    #region Implementation

    /// <summary>
    /// Name of the IR Server plugin.
    /// </summary>
    /// <value>The name.</value>
    public override string Name         { get { return "RedEye Blaster"; } }
    /// <summary>
    /// IR Server plugin version.
    /// </summary>
    /// <value>The version.</value>
    public override string Version      { get { return "1.0.3.5"; } }
    /// <summary>
    /// The IR Server plugin's author.
    /// </summary>
    /// <value>The author.</value>
    public override string Author       { get { return "and-81"; } }
    /// <summary>
    /// A description of the IR Server plugin.
    /// </summary>
    /// <value>The description.</value>
    public override string Description  { get { return "Support for the RedEye serial IR Blaster device"; } }

    /// <summary>
    /// Detect the presence of this device.  Devices that cannot be detected will always return false.
    /// </summary>
    /// <returns>
    /// true if the device is present, otherwise false.
    /// </returns>
    public override bool Detect()
    {
      return false;
    }

    /// <summary>
    /// Start the IR Server plugin.
    /// </summary>
    /// <returns>true if successful, otherwise false.</returns>
    public override bool Start()
    {
      LoadSettings();

      _serialPort           = new SerialPort(_serialPortName, 9600, Parity.None, 8, StopBits.One);
      _serialPort.Handshake = Handshake.None;
      _serialPort.DtrEnable = true;
      _serialPort.RtsEnable = true;

      _serialPort.Open();

      Thread.Sleep(50);

      switch (_blastMode)
      {
        case BlastMode.IRDA:  _serialPort.Write(BlastModeIRDA); break;
        case BlastMode.RC5:   _serialPort.Write(BlastModeRC5);  break;
        case BlastMode.Sky:   _serialPort.Write(BlastModeSky);  break;
        default:                                                break;
      }

      Thread.Sleep(500);

      return true;
    }
    /// <summary>
    /// Suspend the IR Server plugin when computer enters standby.
    /// </summary>
    public override void Suspend()
    {
      Stop();
    }
    /// <summary>
    /// Resume the IR Server plugin when the computer returns from standby.
    /// </summary>
    public override void Resume()
    {
      Start();
    }
    /// <summary>
    /// Stop the IR Server plugin.
    /// </summary>
    public override void Stop()
    {
      if (_serialPort == null)
        return;

      try
      {
        _serialPort.Dispose();
      }
#if TRACE
      catch (Exception ex)
      {
        Trace.WriteLine(ex.ToString());
      }
#else
      catch
      {
      }
#endif
      finally
      {
        _serialPort = null;
      }
    }

    /// <summary>
    /// Configure the IR Server plugin.
    /// </summary>
    public void Configure(IWin32Window owner)
    {
      LoadSettings();

      Configure config = new Configure();
      config.CommPort = _serialPortName;

      if (config.ShowDialog(owner) == DialogResult.OK)
      {
        _serialPortName = config.CommPort;

        SaveSettings();
      }
    }

    /// <summary>
    /// Lists the available blaster ports.
    /// </summary>
    /// <value>The available ports.</value>
    public string[] AvailablePorts
    {
      get { return Ports; }
    }

    /// <summary>
    /// Transmit an infrared command.
    /// </summary>
    /// <param name="port">Port to transmit on (ignored).</param>
    /// <param name="data">Data to transmit.</param>
    /// <returns>true if successful, otherwise false.</returns>
    public bool Transmit(string port, byte[] data)
    {
      if (_serialPort == null)
        return false;

      _serialPort.Write(data, 0, data.Length);

      Thread.Sleep(300);

      return true;
    }

    /// <summary>
    /// Releases unmanaged and - optionally - managed resources
    /// </summary>
    /// <param name="disposing"><c>true</c> to release both managed and unmanaged resources; <c>false</c> to release only unmanaged resources.</param>
    protected virtual void Dispose(bool disposing)
    {
      // process only if mananged and unmanaged resources have
      // not been disposed of.
      if (!_disposed)
      {
        if (disposing)
        {
          // dispose managed resources
          Stop();
        }

        // dispose unmanaged resources
        _disposed = true;
      }
    }

    /// <summary>
    /// Loads the settings.
    /// </summary>
    void LoadSettings()
    {
      try
      {
        XmlDocument doc = new XmlDocument();
        doc.Load(ConfigurationFile);

        _serialPortName = doc.DocumentElement.Attributes["SerialPortName"].Value;
        _blastMode = (BlastMode)Enum.Parse(typeof(BlastMode), doc.DocumentElement.Attributes["BlastMode"].Value);
      }
#if TRACE
      catch (Exception ex)
      {
        Trace.WriteLine(ex.ToString());
#else
      catch
      {
#endif

        _serialPortName = "COM1";
        _blastMode = BlastMode.Sky;
      }
    }
    /// <summary>
    /// Saves the settings.
    /// </summary>
    void SaveSettings()
    {
      try
      {
        using (XmlTextWriter writer = new XmlTextWriter(ConfigurationFile, System.Text.Encoding.UTF8))
        {
          writer.Formatting = Formatting.Indented;
          writer.Indentation = 1;
          writer.IndentChar = (char)9;
          writer.WriteStartDocument(true);
          writer.WriteStartElement("settings"); // <settings>

          writer.WriteAttributeString("SerialPortName", _serialPortName);
          writer.WriteAttributeString("BlastMode", Enum.GetName(typeof(BlastMode), _blastMode));

          writer.WriteEndElement(); // </settings>
          writer.WriteEndDocument();
        }
      }
#if TRACE
      catch (Exception ex)
      {
        Trace.WriteLine(ex.ToString());
      }
#else
      catch
      {
      }
#endif
    }
    
    #endregion Implementation

  }

}
