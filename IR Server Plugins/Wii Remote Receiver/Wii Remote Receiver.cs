using System;
using System.Diagnostics;
using System.Drawing;
using System.Windows.Forms;

using IRServerPluginInterface;

using WiimoteLib;

namespace WiiRemoteReceiver
{

  /// <summary>
  /// IR Server Plugin for the Wii Remote.
  /// </summary>
  public class WiiRemoteReceiver : IRServerPluginBase, IRemoteReceiver, IMouseReceiver
  {

    #region Variables

    RemoteHandler _remoteButtonHandler;
    MouseHandler _mouseHandler;

    Wiimote _wiimote;

    WiimoteState _previousState;

    int screenWidth = Screen.PrimaryScreen.WorkingArea.Width;
    int screenHeight = Screen.PrimaryScreen.WorkingArea.Height;

    #endregion Variables

    #region Implementation

    /// <summary>
    /// Name of the IR Server plugin.
    /// </summary>
    /// <value>The name.</value>
    public override string Name         { get { return "Wii Remote"; } }
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
    public override string Description  { get { return "Supports the Wii Remote"; } }

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
      _wiimote = new Wiimote();

      _wiimote.WiimoteChanged += new WiimoteChangedEventHandler(WiimoteChanged);
      _wiimote.WiimoteExtensionChanged += new WiimoteExtensionChangedEventHandler(WiimoteExtensionChanged);

      _wiimote.Connect();
      _wiimote.SetReportType(Wiimote.InputReport.IRAccel, true);
      _wiimote.SetLEDs(false, true, true, false);
      _wiimote.SetRumble(false);

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
      if (_wiimote == null)
        return;

      _wiimote.SetLEDs(false, false, false, false);
      _wiimote.SetRumble(false);
      _wiimote.Disconnect();
    }

    /// <summary>
    /// Callback for remote button presses.
    /// </summary>
    /// <value>The remote callback.</value>
    public RemoteHandler RemoteCallback
    {
      get { return _remoteButtonHandler; }
      set { _remoteButtonHandler = value; }
    }
    
    /// <summary>
    /// Callback for mouse events.
    /// </summary>
    /// <value>The mouse callback.</value>
    public MouseHandler MouseCallback
    {
      get { return _mouseHandler; }
      set { _mouseHandler = value; }
    }


    void WiimoteChanged(object sender, WiimoteChangedEventArgs args)
    {
      WiimoteState ws = args.WiimoteState;

      if (_previousState != null)
      {
        if (ws.ButtonState.A && !_previousState.ButtonState.A)          RemoteCallback("Wiimote_Button:A");
        if (ws.ButtonState.B && !_previousState.ButtonState.B)          RemoteCallback("Wiimote_Button:B");
        if (ws.ButtonState.Home && !_previousState.ButtonState.Home)    RemoteCallback("Wiimote_Button:Home");
        if (ws.ButtonState.Minus && !_previousState.ButtonState.Minus)  RemoteCallback("Wiimote_Button:Minus");
        if (ws.ButtonState.One && !_previousState.ButtonState.One)      RemoteCallback("Wiimote_Button:One");
        if (ws.ButtonState.Plus && !_previousState.ButtonState.Plus)    RemoteCallback("Wiimote_Button:Plus");
        if (ws.ButtonState.Two && !_previousState.ButtonState.Two)      RemoteCallback("Wiimote_Button:Two");

        if (ws.ButtonState.Down && !_previousState.ButtonState.Down)    RemoteCallback("Wiimote_Pad:Down");
        if (ws.ButtonState.Left && !_previousState.ButtonState.Left)    RemoteCallback("Wiimote_Pad:Left");
        if (ws.ButtonState.Right && !_previousState.ButtonState.Right)  RemoteCallback("Wiimote_Pad:Right");
        if (ws.ButtonState.Up && !_previousState.ButtonState.Up)        RemoteCallback("Wiimote_Pad:Up");

        if (ws.ExtensionType == ExtensionType.Nunchuk)
        {
          if (ws.NunchukState.C && !_previousState.NunchukState.C)      RemoteCallback("WiimoteNunchuk_Button:C");
          if (ws.NunchukState.Z && !_previousState.NunchukState.Z)      RemoteCallback("WiimoteNunchuk_Button:Z");
        }

        if (ws.ExtensionType == ExtensionType.ClassicController)
        {
          if (ws.ClassicControllerState.ButtonState.A && !_previousState.ClassicControllerState.ButtonState.A)                RemoteCallback("WiimoteClassic_Button:A");
          if (ws.ClassicControllerState.ButtonState.B && !_previousState.ClassicControllerState.ButtonState.B)                RemoteCallback("WiimoteClassic_Button:B");
          if (ws.ClassicControllerState.ButtonState.Home && !_previousState.ClassicControllerState.ButtonState.Home)          RemoteCallback("WiimoteClassic_Button:Home");
          if (ws.ClassicControllerState.ButtonState.Minus && !_previousState.ClassicControllerState.ButtonState.Minus)        RemoteCallback("WiimoteClassic_Button:Minus");
          if (ws.ClassicControllerState.ButtonState.Plus && !_previousState.ClassicControllerState.ButtonState.Plus)          RemoteCallback("WiimoteClassic_Button:Plus");
          if (ws.ClassicControllerState.ButtonState.X && !_previousState.ClassicControllerState.ButtonState.X)                RemoteCallback("WiimoteClassic_Button:X");
          if (ws.ClassicControllerState.ButtonState.Y && !_previousState.ClassicControllerState.ButtonState.Y)                RemoteCallback("WiimoteClassic_Button:Y");
          if (ws.ClassicControllerState.ButtonState.TriggerL && !_previousState.ClassicControllerState.ButtonState.TriggerL)  RemoteCallback("WiimoteClassic_Button:TriggerL");
          if (ws.ClassicControllerState.ButtonState.TriggerR && !_previousState.ClassicControllerState.ButtonState.TriggerR)  RemoteCallback("WiimoteClassic_Button:TriggerR");
          if (ws.ClassicControllerState.ButtonState.ZL && !_previousState.ClassicControllerState.ButtonState.ZL)              RemoteCallback("WiimoteClassic_Button:ZL");
          if (ws.ClassicControllerState.ButtonState.ZR && !_previousState.ClassicControllerState.ButtonState.ZR)              RemoteCallback("WiimoteClassic_Button:ZR");

          if (ws.ClassicControllerState.ButtonState.Down && !_previousState.ClassicControllerState.ButtonState.Down)          RemoteCallback("WiimoteClassic_Pad:Down");
          if (ws.ClassicControllerState.ButtonState.Left && !_previousState.ClassicControllerState.ButtonState.Left)          RemoteCallback("WiimoteClassic_Pad:Left");
          if (ws.ClassicControllerState.ButtonState.Right && !_previousState.ClassicControllerState.ButtonState.Right)        RemoteCallback("WiimoteClassic_Pad:Right");
          if (ws.ClassicControllerState.ButtonState.Up && !_previousState.ClassicControllerState.ButtonState.Up)              RemoteCallback("WiimoteClassic_Pad:Up");
        }

        if (ws.IRState.Found1 && ws.IRState.Found2)
        {
          int x = (int)(screenWidth - (ws.IRState.X1 + ws.IRState.X2) / 2 * screenWidth);
          int y = (int)((ws.IRState.Y1 + ws.IRState.Y2) / 2 * screenHeight);

          if (_mouseHandler == null)
          {
            Cursor.Position = new Point(x, y);
          }
          else
          {
            int prevX = (int)(screenWidth - (_previousState.IRState.X1 + _previousState.IRState.X2) / 2 * screenWidth);
            int prevY = (int)((_previousState.IRState.Y1 + _previousState.IRState.Y2) / 2 * screenHeight);

            int deltaX = x - prevX;
            int deltaY = y - prevY;

            MouseCallback(deltaX, deltaY, 0);
          }
        }
      }
      else
        _previousState = new WiimoteState();
      
      //_previousState.AccelCalibrationInfo.X0 = ws.AccelCalibrationInfo.X0;
      //_previousState.AccelCalibrationInfo.XG = ws.AccelCalibrationInfo.XG;
      //_previousState.AccelCalibrationInfo.Y0 = ws.AccelCalibrationInfo.Y0;
      //_previousState.AccelCalibrationInfo.YG = ws.AccelCalibrationInfo.YG;
      //_previousState.AccelCalibrationInfo.Z0 = ws.AccelCalibrationInfo.Z0;
      //_previousState.AccelCalibrationInfo.ZG = ws.AccelCalibrationInfo.ZG;

      //_previousState.AccelState.RawX = ws.AccelState.RawX;
      //_previousState.AccelState.RawY = ws.AccelState.RawY;
      //_previousState.AccelState.RawZ = ws.AccelState.RawZ;
      //_previousState.AccelState.X = ws.AccelState.X;
      //_previousState.AccelState.Y = ws.AccelState.Y;
      //_previousState.AccelState.Z = ws.AccelState.Z;

      //_previousState.Battery = ws.Battery;

      _previousState.ButtonState.A = ws.ButtonState.A;
      _previousState.ButtonState.B = ws.ButtonState.B;
      _previousState.ButtonState.Down = ws.ButtonState.Down;
      _previousState.ButtonState.Home = ws.ButtonState.Home;
      _previousState.ButtonState.Left = ws.ButtonState.Left;
      _previousState.ButtonState.Minus = ws.ButtonState.Minus;
      _previousState.ButtonState.One = ws.ButtonState.One;
      _previousState.ButtonState.Plus = ws.ButtonState.Plus;
      _previousState.ButtonState.Right = ws.ButtonState.Right;
      _previousState.ButtonState.Two = ws.ButtonState.Two;
      _previousState.ButtonState.Up = ws.ButtonState.Up;

      _previousState.Extension = ws.Extension;
      _previousState.ExtensionType = ws.ExtensionType;
      
      _previousState.IRState.Found1 = ws.IRState.Found1;
      _previousState.IRState.Found2 = ws.IRState.Found2;
      _previousState.IRState.MidX = ws.IRState.MidX;
      _previousState.IRState.MidY = ws.IRState.MidY;
      _previousState.IRState.Mode = ws.IRState.Mode;
      _previousState.IRState.RawMidX = ws.IRState.RawMidX;
      _previousState.IRState.RawMidY = ws.IRState.RawMidY;
      _previousState.IRState.RawX1 = ws.IRState.RawX1;
      _previousState.IRState.RawX2 = ws.IRState.RawX2;
      _previousState.IRState.RawY1 = ws.IRState.RawY1;
      _previousState.IRState.RawY2 = ws.IRState.RawY2;
      _previousState.IRState.Size1 = ws.IRState.Size1;
      _previousState.IRState.Size2 = ws.IRState.Size2;
      _previousState.IRState.X1 = ws.IRState.X1;
      _previousState.IRState.X2 = ws.IRState.X2;
      _previousState.IRState.Y1 = ws.IRState.Y1;
      _previousState.IRState.Y2 = ws.IRState.Y2;

      _previousState.NunchukState.C = ws.NunchukState.C;
      _previousState.NunchukState.Z = ws.NunchukState.Z;

    }

    void WiimoteExtensionChanged(object sender, WiimoteExtensionChangedEventArgs args)
    {
      if (args.Inserted)
        _wiimote.SetReportType(Wiimote.InputReport.IRExtensionAccel, true);
      else
        _wiimote.SetReportType(Wiimote.InputReport.IRAccel, true);
    }
        

    #endregion Implementation

  }

}
