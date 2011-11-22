#region Copyright (C) 2005-2009 Team MediaPortal

// Copyright (C) 2005-2009 Team MediaPortal
// http://www.team-mediaportal.com
// 
// This Program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
// 
// This Program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with GNU Make; see the file COPYING.  If not, write to
// the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
// http://www.gnu.org/copyleft/gpl.html

#endregion

using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.ComponentModel;
using System.Collections;
using System.Runtime.InteropServices;
using System.Xml;
using System.Diagnostics;
using IrssComms;
using IrssUtils;
using IrssUtils.Forms;
using MediaPortal.Configuration;
using MediaPortal.GUI.Library;
using MediaPortal.Hardware;
using MediaPortal.Profile;
using Microsoft.Win32;
using MPUtils;

namespace MediaPortal.Plugins
{
  [PluginIcons("MediaPortal.Plugins.MPControlPlugin.IRSS.iconGreen.gif",
    "MediaPortal.Plugins.MPControlPlugin.IRSS.iconGray.gif")]

  /// <summary>
    /// MediaPortal Control Plugin for IR Server.
    /// </summary>
  [CLSCompliant(false)]
  public class MPControlPlugin : IPlugin, ISetupForm
  {
    #region Constants

    /// <summary>
    /// The plugin version string.
    /// </summary>
    internal const string PluginVersion = "MP Control Plugin 1.4.2.0 for IR Server";

    private const string ProcessCommandThreadName = "ProcessCommand";

    internal static readonly string EventMappingFile = Path.Combine(IrssUtils.Common.FolderAppData,
                                                                    "MP Control Plugin\\EventMapping.xml");

    internal static readonly string FolderMacros = Path.Combine(IrssUtils.Common.FolderAppData, "MP Control Plugin\\Macro");

    internal static readonly string MultiMappingFile = Path.Combine(IrssUtils.Common.FolderAppData,
                                                                    "MP Control Plugin\\MultiMapping.xml");

    internal static readonly string RemotePresetsFolder = Path.Combine(IrssUtils.Common.FolderAppData,
                                                                       "MP Control Plugin\\Remote Presets");

    internal static readonly string RemotesFile = Path.Combine(IrssUtils.Common.FolderAppData, "MP Control Plugin\\Remotes.xml");

    #endregion Constants

    #region Variables

    private static Client _client;
    private static InputHandler _defaultInputHandler;
    private static bool _eventMapperEnabled;
    private static List<MappedEvent> _eventMappings;
    private static ClientMessageSink _handleMessage;

    private static bool _inConfiguration;
    private static IRServerInfo _irServerInfo = new IRServerInfo();

    private static string _learnIRFilename;

    private static bool _mouseModeAcceleration;

    private static bool _mouseModeActive;
    private static RemoteButton_my _mouseModeButton = RemoteButton_my.None;
    private static bool _mouseModeEnabled;
    private static RemoteButton_my _mouseModeLastButton = RemoteButton_my.None;
    private static long _mouseModeLastButtonTicks;
    private static bool _mouseModeLeftHeld;
    private static bool _mouseModeMiddleHeld;
    private static int _mouseModeRepeatCount;
    private static bool _mouseModeRightHeld;
    private static int _mouseModeStep;
    private static bool _mpBasicHome;
    private static List<InputHandler> _multiInputHandlers;

    private static RemoteButton_my _multiMappingButton;
    private static bool _multiMappingEnabled;
    private static int _multiMappingSet;
    private static string[] _multiMaps;
    private static bool _registered;

    private static MappedKeyCode[] _remoteMap;
    private static bool _requireFocus;
    private static string _serverHost;

    private ReceiverWindow _receiverWindowHID;
    private RawInput.RAWINPUTDEVICE[] _deviceTree;
    private int KeyboardDevice = -1;
    private string KeyboardDeviceName = string.Empty;
    private static readonly string[] SupportedDevices_HID = new string[]
        {
            "Vid_15c2&Pid_003c",
            "Vid_15c2&Pid_0038",
            "Vid_15c2&Pid_0036"
        };
    private const string HIDKeyboardSuffix = "MI_00&Col02#";
    private const int WM_KEYDOWN = 0x0100;
    private const int WM_SYSKEYDOWN = 0x0104;
    private const int WM_KEYUP = 0x0101;
    private const int WM_SYSKEYUP = 0x0105;
    [DllImport("user32.dll")]
    private static extern bool GetKeyboardState(byte[] keyState);
    [DllImport("user32.dll", CharSet = CharSet.Auto)]
    internal static extern int MapVirtualKey(int uCode, int nMapType);
    [DllImport("user32.dll")]
    public static extern bool PostMessage(IntPtr hwnd, uint wMsg, uint wParam, uint lParam);
    [DllImport("user32.dll")]
    static extern bool SetKeyboardState(byte[] lpKeyState);
    // Define all possible Ctrl-keys
    private static Hashtable controlKeys = getAsHashtable(new Keys[] { Keys.ControlKey, Keys.LControlKey, Keys.RControlKey });
    // Define all possible Alt-keys
    private static Hashtable menuKeys = getAsHashtable(new Keys[] { Keys.Menu, Keys.LMenu, Keys.RMenu });
    // Define all possible Shift-keys
    private static Hashtable shiftKeys = getAsHashtable(new Keys[] { Keys.ShiftKey, Keys.LShiftKey, Keys.RShiftKey });
    // Define all possible Win-keys
    //private static Hashtable winKeys = getAsHashtable(new Keys[] { Keys.LWin, Keys.RWin });
    // Define all 'special' modifier keys; modifier keys that are not defined via Control.ModifierKeys
    // Currently these are only the Win-keys
    //private static Hashtable specialModifierKeys = combineTables(new Hashtable[] { winKeys });
    // Define all possible modifier keys, being all Ctrl, Alt, Shift and Win-keys
    //private static Hashtable modifierKeys = combineTables(new Hashtable[] { controlKeys, menuKeys, shiftKeys, winKeys });
    private static Hashtable modifierKeys = combineTables(new Hashtable[] { controlKeys, menuKeys, shiftKeys });
    // The currently pressed modifier keys
    private static Hashtable currentModifierKeys = new Hashtable();

    #endregion Variables
     
    #region Hashtable / Set utility methods.

    private static Hashtable getAsHashtable(Keys[] keys)
    {
        Hashtable result = new Hashtable();
        foreach (object key in keys)
        {
            result.Add(key, null);
        }
        return result;
    }

    private static Hashtable combineTables(Hashtable[] hashtables)
    {
        Hashtable result = new Hashtable();
        foreach (Hashtable hashtable in hashtables)
        {
            foreach (object key in hashtable.Keys)
            {
                result.Add(key, hashtable[key]);
            }
        }
        return result;
    }

    protected bool Overlaps(Hashtable table1, Hashtable table2)
    {
        foreach (object key in table1.Keys)
        {
            if (table2.ContainsKey(key)) return true;
        }
        return false;
    }

    #endregion

    #region Properties

    internal static string ServerHost
    {
      get { return _serverHost; }
      set { _serverHost = value; }
    }

    /// <summary>
    /// Gets or sets a value indicating whether MediaPortal will require focus to handle input.
    /// </summary>
    /// <value><c>true</c> if requires focus; otherwise, <c>false</c>.</value>
    internal static bool RequireFocus
    {
      get { return _requireFocus; }
      set { _requireFocus = value; }
    }

    /// <summary>
    /// Gets or sets a value indicating whether multi mapping is enabled.
    /// </summary>
    /// <value><c>true</c> if multi mapping is enabled; otherwise, <c>false</c>.</value>
    internal static bool MultiMappingEnabled
    {
      get { return _multiMappingEnabled; }
      set { _multiMappingEnabled = value; }
    }

    /// <summary>
    /// Gets or sets a value indicating whether the event mapper is enabled.
    /// </summary>
    /// <value><c>true</c> if the event mapper is enabled; otherwise, <c>false</c>.</value>
    internal static bool EventMapperEnabled
    {
      get { return _eventMapperEnabled; }
      set { _eventMapperEnabled = value; }
    }

    /// <summary>
    /// Gets or sets the mouse mode button.
    /// </summary>
    /// <value>The mouse mode button.</value>
    internal static RemoteButton_my MouseModeButton
    {
      get { return _mouseModeButton; }
      set { _mouseModeButton = value; }
    }

    /// <summary>
    /// Gets or sets a value indicating whether mouse mode is enabled.
    /// </summary>
    /// <value><c>true</c> if mouse mode is enabled; otherwise, <c>false</c>.</value>
    internal static bool MouseModeEnabled
    {
      get { return _mouseModeEnabled; }
      set { _mouseModeEnabled = value; }
    }

    /// <summary>
    /// Gets or sets a value indicating whether mouse mode is active.
    /// </summary>
    /// <value><c>true</c> if mouse mode is active; otherwise, <c>false</c>.</value>
    internal static bool MouseModeActive
    {
      get { return _mouseModeActive; }
      set { _mouseModeActive = value; }
    }

    /// <summary>
    /// Gets or sets the mouse mode step distance.
    /// </summary>
    /// <value>The mouse mode step distance.</value>
    internal static int MouseModeStep
    {
      get { return _mouseModeStep; }
      set { _mouseModeStep = value; }
    }

    /// <summary>
    /// Gets or sets a value indicating whether mouse mode acceleration is enabled.
    /// </summary>
    /// <value>
    /// <c>true</c> if mouse mode acceleration is enabled; otherwise, <c>false</c>.
    /// </value>
    internal static bool MouseModeAcceleration
    {
      get { return _mouseModeAcceleration; }
      set { _mouseModeAcceleration = value; }
    }

    /// <summary>
    /// Gets the event mappings.
    /// </summary>
    /// <value>The event mappings.</value>
    internal static List<MappedEvent> EventMappings
    {
      get { return _eventMappings; }
    }

    /// <summary>
    /// Gets or sets the multi mapping button.
    /// </summary>
    /// <value>The multi mapping button.</value>
    internal static RemoteButton_my MultiMappingButton
    {
      get { return _multiMappingButton; }
      set { _multiMappingButton = value; }
    }

    /// <summary>
    /// Gets the multi maps.
    /// </summary>
    /// <value>The multi maps.</value>
    internal static string[] MultiMaps
    {
      get { return _multiMaps; }
    }

    /// <summary>
    /// Gets or sets a value indicating whether in configuration.
    /// </summary>
    /// <value><c>true</c> if in configuration; otherwise, <c>false</c>.</value>
    internal static bool InConfiguration
    {
      get { return _inConfiguration; }
      set { _inConfiguration = value; }
    }

    internal static ClientMessageSink HandleMessage
    {
      get { return _handleMessage; }
      set { _handleMessage = value; }
    }

    internal static IRServerInfo TransceiverInformation
    {
      get { return _irServerInfo; }
    }

    /// <summary>
    /// Gets a value indicating whether MediaPortal has basic home enabled.
    /// </summary>
    /// <value><c>true</c> if MediaPortal has basic home enabled; otherwise, <c>false</c>.</value>
    internal static bool MP_BasicHome
    {
      get { return _mpBasicHome; }
    }

    #endregion Properties
      
    #region Procmessages
       private void ProcMessage(ref Message m)
        {
            switch (m.Msg)
            {
                case RawInput.WM_INPUT:
                    m.Result = (IntPtr)1;
                    ProcessInputCommand(ref m);
                    break;
            }
        }
       private void ProcessInputCommand(ref Message message)
       {
           uint dwSize = 0;

           // if no imon HID Keyboard was not we do not have to do anything
           if (KeyboardDevice < 0) return;

           RawInput.GetRawInputData(message.LParam, RawInput.RawInputCommand.Input, IntPtr.Zero, ref dwSize,
                                    (uint)Marshal.SizeOf(typeof(RawInput.RAWINPUTHEADER)));

           IntPtr buffer = Marshal.AllocHGlobal((int)dwSize);
           try
           {
               if (buffer == IntPtr.Zero)
                   return;

               if (RawInput.GetRawInputData(message.LParam, RawInput.RawInputCommand.Input, buffer, ref dwSize,
                                          (uint)Marshal.SizeOf(typeof(RawInput.RAWINPUTHEADER))) != dwSize)
                   return;

               RawInput.RAWINPUT raw = (RawInput.RAWINPUT)Marshal.PtrToStructure(buffer, typeof(RawInput.RAWINPUT));

               #region device filtering
               // get the name of the device that generated the input message
               string deviceName = string.Empty;
               uint pcbSize = 0;
               RawInput.GetRawInputDeviceInfo(raw.header.hDevice, RawInput.RIDI_DEVICENAME, IntPtr.Zero, ref pcbSize);
               if (pcbSize > 0)
               {
                   IntPtr pData = Marshal.AllocHGlobal((int)pcbSize);
                   RawInput.GetRawInputDeviceInfo(raw.header.hDevice, RawInput.RIDI_DEVICENAME, pData, ref pcbSize);
                   deviceName = Marshal.PtrToStringAnsi(pData);
                   Marshal.FreeHGlobal(pData);
               }

               // OK here we have to process all other WM_INPUT Keyboard Msgs and ONLY Keyboard Messages cause we only register for Keyboards
               if (raw.header.dwType == RawInput.RawInputType.Keyboard)
               {
                   // stop processing if the device that generated the event IS iMon Keyboard device
                   if (deviceName.Equals(KeyboardDeviceName)) return;
                   Log.Debug("MPControlPlugin: Received WM_INPUT Keyboard Message from: {0} Msg: {1} Make: {2} Reserved: {3} Flags: {4}, Extra: {5}, VKey: {6}",
                              deviceName, raw.keyboard.Message.ToString(), raw.keyboard.MakeCode.ToString(), raw.keyboard.Reserved.ToString(),
                              raw.keyboard.Flags.ToString(), raw.keyboard.ExtraInformation.ToString(),
                              raw.keyboard.VKey.ToString());

                   Keys key = (Keys)raw.keyboard.VKey;
                   if (raw.keyboard.Message == WM_SYSKEYDOWN || raw.keyboard.Message == WM_KEYDOWN)
                   {
                       if (modifierKeys.ContainsKey(key))
                       {
                           // Handle modifier keys; add these keys to the currentModifierKeys set.
                           if (!currentModifierKeys.ContainsKey(key))
                           {
                               currentModifierKeys.Add(key, null);
                           }
                       }
                       else
                       {
                           // real Key Press - so send it to the window
                           PostKeyExHWND(GUIGraphicsContext.form.Handle, key, ref currentModifierKeys);
                       }
                   }
                   else
                   {
                       // must be a key up Message
                       if (modifierKeys.ContainsKey(key) && currentModifierKeys.ContainsKey(key))
                       {
                           currentModifierKeys.Remove(key);
                       }
                   }
               }

               #endregion
           }
           finally
           {
               Marshal.FreeHGlobal(buffer);
           }
       }
       public int MAKELONG(ushort lowPart, ushort highPart)
       {
           return (int)(((ushort)lowPart) | (uint)(highPart << 16));
       }

        public void PostKeyExHWND(IntPtr hWindow, Keys key, ref Hashtable currentModifierKeys) {
            byte[] kbState = new byte[255];
            int lParam = MAKELONG(0, (ushort)MapVirtualKey((int)key, 0));
            GetKeyboardState(kbState);
            byte[] kbStateMOD = kbState;

            if (Overlaps(currentModifierKeys, shiftKeys)) { 
                kbStateMOD[(int)Keys.ShiftKey] = 0x80;
            }
            if (Overlaps(currentModifierKeys, menuKeys)) {
                kbStateMOD[(int)Keys.Menu] = 0x80;
                lParam = lParam | 0x20000000;
            }
            if (Overlaps(currentModifierKeys, controlKeys)) {
                kbStateMOD[(int)Keys.ControlKey] = 0x80;
            }

            SetKeyboardState(kbStateMOD);
            if (Overlaps(currentModifierKeys, menuKeys)) {
                Log.Debug("MPControlPlugin: send SYSKEY: key {0} lparam: {1}", key.ToString(), lParam.ToString());
                PostMessage(hWindow, WM_SYSKEYDOWN, (uint)key, (uint)lParam);
                PostMessage(hWindow, WM_SYSKEYUP, (uint)key, ((uint)lParam | 0xC0000000));
            }
            else
            {
                Log.Debug("MPControlPlugin: send KEY: key {0} lparam: {1}", key.ToString(), lParam.ToString());
                PostMessage(hWindow, WM_KEYDOWN, (uint)key, (uint)lParam);
                PostMessage(hWindow, WM_KEYUP, (uint)key, ((uint)lParam | 0xC0000000));
            }
            Application.DoEvents();

            SetKeyboardState(kbState);
        }

      #region RawInputfunctions
      private void FindDevices_HID()
        {
            if (_deviceTree != null) return;
            // configure the device tree
            int numDevices = 1;
            Log.Debug("MPControlPlugin: FindDevices_HID(): searching for {0} devices", numDevices);
            if (numDevices == 0) return;
            RawInput.RAWINPUTDEVICE kDevice = new RawInput.RAWINPUTDEVICE();
            // get the complete list of raw input devices and parse it for supported devices
            List<DeviceDetails> _devices = new List<DeviceDetails>();

            try
            {
                _devices = RawInput.EnumerateDevices();
            }
            catch
            {
                return;
            }

            if (_devices.Count > 0)
            {
                foreach (DeviceDetails details in _devices)
                {
                    Log.Debug("MPControlPlugin: FindDevices_HID(): checking device \"{0}\"", details.ID);
                    // check the details against the supported device list
                    foreach (string sDevice in SupportedDevices_HID)
                    {
                            // check for keyboard device - MI_00&Col02#
                            if (details.ID.Contains(HIDKeyboardSuffix))
                            {
                                Log.Debug("MPControlPlugin: FindDevices_HID(): Found iMon Keyboard device\n");
                                // found the keyboard device
                                kDevice = new RawInput.RAWINPUTDEVICE();
                                kDevice.usUsage = details.Usage;
                                kDevice.usUsagePage = details.UsagePage;
                                KeyboardDeviceName = details.ID;
                            }
                        }
                    }
                }
                numDevices = ((kDevice.usUsage > 0) ? 2 : 0);
                int DevIndex = 0;
                Log.Debug("MPControlPlugin: FindDevices_HID(): Found {0} Devices", numDevices);
                _deviceTree = new RawInput.RAWINPUTDEVICE[numDevices];

                if (kDevice.usUsage > 0)
                {
                    KeyboardDevice = DevIndex;
                    DevIndex++;
                    _deviceTree[KeyboardDevice].usUsage = kDevice.usUsage;
                    _deviceTree[KeyboardDevice].usUsagePage = kDevice.usUsagePage;

                    _deviceTree[DevIndex].usUsage = kDevice.usUsage;
                    _deviceTree[DevIndex].usUsagePage = kDevice.usUsagePage;
                    DevIndex++;
                    Log.Debug("MPControlPlugin: FindDevices_HID(): Added iMon Keyboard device as deviceTree[{0}]", KeyboardDevice);
                }

            }
              private bool RegisterForRawInput(RawInput.RAWINPUTDEVICE device)
              {
                  RawInput.RAWINPUTDEVICE[] devices = new RawInput.RAWINPUTDEVICE[1];
                  devices[0] = device;

                  return RegisterForRawInput(devices);
              }
              private bool RegisterForRawInput(RawInput.RAWINPUTDEVICE[] devices)
              {
                  Log.Debug("MPControlPlugin: RegisterForRawInput(): Registering {0} device(s).", devices.Length);
                  if (
                    !RawInput.RegisterRawInputDevices(devices, (uint)devices.Length,
                                                      (uint)Marshal.SizeOf(typeof(RawInput.RAWINPUTDEVICE))))
                  {
                      int dwError = Marshal.GetLastWin32Error();
                      Log.Debug("MPControlPlugin: RegisterForRawInput(): error={0}", dwError);
                      throw new Win32Exception(dwError, "Imon:RegisterForRawInput()");
                  }
                  Log.Debug("MPControlPlugin: RegisterForRawInput(): Done.");
                  return true;
              }
    #endregion

    #region IPlugin methods

    /// <summary>
    /// Starts this instance.
    /// </summary>
    public void Start()
    {
      _inConfiguration = false;

      Log.Info("MPControlPlugin: Starting ({0})", PluginVersion);

      // Load basic settings
      LoadSettings();

      // Load the remote button mappings
      _remoteMap = LoadRemoteMap(RemotesFile);

      // Find Imon HID Device if we have one
      FindDevices_HID();
      if (KeyboardDevice > -1)
      {
          Log.Debug("MPControlPlugin: Keyboard Usage: {0}", _deviceTree[KeyboardDevice].usUsage);
          Log.Debug("MPControlPlugin: Keyboard UsagePage: {0}", _deviceTree[KeyboardDevice].usUsagePage);

          _receiverWindowHID = new ReceiverWindow("WM_INPUT receiver");
          _receiverWindowHID.ProcMsg += ProcMessage;

          _deviceTree[KeyboardDevice].dwFlags = RawInput.RawInputDeviceFlags.NoLegacy |
                                        RawInput.RawInputDeviceFlags.InputSink;

          _deviceTree[KeyboardDevice].hwndTarget = GUIGraphicsContext.form.Handle;

          _deviceTree[KeyboardDevice + 1].dwFlags = RawInput.RawInputDeviceFlags.NoLegacy |
                                        RawInput.RawInputDeviceFlags.InputSink;

          _deviceTree[KeyboardDevice + 1].hwndTarget = _receiverWindowHID.Handle;

          if (!RegisterForRawInput(_deviceTree))
          {
              Log.Debug("MPControlPlugin: ERROR: Failed to register for HID Raw input");
              throw new InvalidOperationException("Failed to register for HID Raw input");
          }
      }
      // Load input handler
      LoadDefaultMapping();

      // Load multi-mappings
      if (MultiMappingEnabled)
        LoadMultiMappings();

      IPAddress serverIP = Network.GetIPFromName(_serverHost);
      IPEndPoint endPoint = new IPEndPoint(serverIP, Server.DefaultPort);

      if (!StartClient(endPoint))
        Log.Error("MPControlPlugin: Failed to start local comms, IR input and IR blasting is disabled for this session");

      // Load the event mapper mappings
      if (EventMapperEnabled)
      {
        LoadEventMappings();

        MapEvent(MappedEvent.MappingEvent.MediaPortal_Start);

        // Register with MediaPortal to receive GUI Messages ...
        GUIWindowManager.Receivers += OnMessage;
      }

      // Register for Power State message ...
      //SystemEvents.SessionEnding += new SessionEndingEventHandler(SystemEvents_SessionEnding);
      SystemEvents.PowerModeChanged += SystemEvents_PowerModeChanged;

      Log.Debug("MPControlPlugin: Started");
    }

    /// <summary>
    /// Stops this instance.
    /// </summary>
    public void Stop()
    {
      SystemEvents.PowerModeChanged -= SystemEvents_PowerModeChanged;

      if (_deviceTree != null && _deviceTree.Length == 2)
      {
          Log.Debug("MPControlPlugin: Stop Imon Keybord HID");
          _deviceTree[0].dwFlags |= RawInput.RawInputDeviceFlags.Remove;
          _deviceTree[1].dwFlags |= RawInput.RawInputDeviceFlags.Remove;
          // this should work but it does not - so perhaps somebody can fix it
          // but it is not really needed - cause when the windows are closed - the RawInputdevices
          // are removed by windows automatically
          // RegisterForRawInput(_deviceTree);

          _receiverWindowHID.ProcMsg -= ProcMessage;
          _receiverWindowHID.DestroyHandle();
          _receiverWindowHID = null;
      }

      if (EventMapperEnabled)
      {
        GUIWindowManager.Receivers -= OnMessage;

        MapEvent(MappedEvent.MappingEvent.MediaPortal_Stop);
      }

      StopClient();

      _defaultInputHandler = null;

      if (MultiMappingEnabled)
        for (int i = 0; i < _multiInputHandlers.Count; i++)
          _multiInputHandlers[i] = null;

      Log.Debug("MPControlPlugin: Stopped");
    }

    #endregion IPlugin methods

    #region ISetupForm methods

    /// <summary>
    /// Determines whether this plugin can be enabled.
    /// </summary>
    /// <returns>
    /// <c>true</c> if this plugin can be enabled; otherwise, <c>false</c>.
    /// </returns>
    public bool CanEnable()
    {
      return true;
    }

    /// <summary>
    /// Determines whether this plugin has setup.
    /// </summary>
    /// <returns>
    /// <c>true</c> if this plugin has setup; otherwise, <c>false</c>.
    /// </returns>
    public bool HasSetup()
    {
      return true;
    }

    /// <summary>
    /// Gets the plugin name.
    /// </summary>
    /// <returns>The plugin name.</returns>
    public string PluginName()
    {
      return "MP Control Plugin for IR Server";
    }

    /// <summary>
    /// Defaults enabled.
    /// </summary>
    /// <returns>true if this plugin is enabled by default, otherwise false.</returns>
    public bool DefaultEnabled()
    {
      return true;
    }

    /// <summary>
    /// Gets the window id.
    /// </summary>
    /// <returns>The window id.</returns>
    public int GetWindowId()
    {
      return 0;
    }

    /// <summary>
    /// Gets the plugin author.
    /// </summary>
    /// <returns>The plugin author.</returns>
    public string Author()
    {
      return "and-81";
    }

    /// <summary>
    /// Gets the description of the plugin.
    /// </summary>
    /// <returns>The plugin description.</returns>
    public string Description()
    {
      return "This plugin uses the IR Server to replace MediaPortal's native remote control support";
    }

    /// <summary>
    /// Shows the plugin configuration.
    /// </summary>
    public void ShowPlugin()
    {
      try
      {
        LoadSettings();
        LoadDefaultMapping();
        LoadMultiMappings();

        _inConfiguration = true;

        Log.Debug("MPControlPlugin: ShowPlugin()");

        SetupForm setupForm = new SetupForm();
        if (setupForm.ShowDialog() == DialogResult.OK)
          SaveSettings();

        StopClient();
        
        Log.Debug("MPControlPlugin: ShowPlugin() - End");
      }
      catch (Exception ex)
      {
        Log.Error(ex);
      }
    }

    /// <summary>
    /// Gets the home screen details for the plugin.
    /// </summary>
    /// <param name="strButtonText">The button text.</param>
    /// <param name="strButtonImage">The button image.</param>
    /// <param name="strButtonImageFocus">The button image focus.</param>
    /// <param name="strPictureImage">The picture image.</param>
    /// <returns>true if the plugin can be seen, otherwise false.</returns>
    public bool GetHome(out string strButtonText, out string strButtonImage, out string strButtonImageFocus,
                        out string strPictureImage)
    {
      strButtonText = strButtonImage = strButtonImageFocus = strPictureImage = String.Empty;
      return false;
    }

    #endregion ISetupForm methods

    #region Implementation

    /// <summary>
    /// Handles the mouse mode.
    /// </summary>
    /// <param name="button">The button pressed.</param>
    /// <returns>true if handled successfully, otherwise false.</returns>
    private static bool HandleMouseMode(RemoteButton_my button)
    {
      if (button == MouseModeButton)
      {
        MouseModeActive = !MouseModeActive; // Toggle Mouse Mode

        string notifyMessage;

        if (MouseModeActive)
        {
          notifyMessage = "Mouse Mode is now ON";
        }
        else
        {
          notifyMessage = "Mouse Mode is now OFF";

          if (_mouseModeLeftHeld)
            Mouse.Button(Mouse.MouseEvents.LeftUp);

          if (_mouseModeRightHeld)
            Mouse.Button(Mouse.MouseEvents.RightUp);

          if (_mouseModeMiddleHeld)
            Mouse.Button(Mouse.MouseEvents.MiddleUp);

          _mouseModeLeftHeld = false;
          _mouseModeRightHeld = false;
          _mouseModeMiddleHeld = false;
        }

        MPCommon.ShowNotifyDialog("Mouse Mode", notifyMessage, 2);

        Log.Debug("MPControlPlugin: {0}", notifyMessage);

        return true;
      }
      else if (MouseModeActive)
      {
        // Determine repeat count ...
        long ticks = DateTime.Now.Ticks;
        if (button != _mouseModeLastButton || new TimeSpan(ticks - _mouseModeLastButtonTicks).Milliseconds >= 500)
          _mouseModeRepeatCount = 0;
        else
          _mouseModeRepeatCount++;

        _mouseModeLastButtonTicks = ticks;
        _mouseModeLastButton = button;


        int distance = MouseModeStep;

        if (MouseModeAcceleration)
          distance += (2 * _mouseModeRepeatCount);

        switch (button)
        {
          case RemoteButton_my.Up:
            Mouse.Move(0, -distance, false);
            return true;

          case RemoteButton_my.Down:
            Mouse.Move(0, distance, false);
            return true;

          case RemoteButton_my.Left:
            Mouse.Move(-distance, 0, false);
            return true;

          case RemoteButton_my.Right:
            Mouse.Move(distance, 0, false);
            return true;

          case RemoteButton_my.Replay: // Left Single-Click
            if (_mouseModeLeftHeld)
              Mouse.Button(Mouse.MouseEvents.LeftUp);

            if (_mouseModeRightHeld)
              Mouse.Button(Mouse.MouseEvents.RightUp);

            if (_mouseModeMiddleHeld)
              Mouse.Button(Mouse.MouseEvents.MiddleUp);

            _mouseModeLeftHeld = false;
            _mouseModeRightHeld = false;
            _mouseModeMiddleHeld = false;

            Mouse.Button(Mouse.MouseEvents.LeftDown);
            Mouse.Button(Mouse.MouseEvents.LeftUp);
            return true;

          case RemoteButton_my.Skip: // Right Single-Click
            if (_mouseModeLeftHeld)
              Mouse.Button(Mouse.MouseEvents.LeftUp);

            if (_mouseModeRightHeld)
              Mouse.Button(Mouse.MouseEvents.RightUp);

            if (_mouseModeMiddleHeld)
              Mouse.Button(Mouse.MouseEvents.MiddleUp);

            _mouseModeLeftHeld = false;
            _mouseModeRightHeld = false;
            _mouseModeMiddleHeld = false;

            Mouse.Button(Mouse.MouseEvents.RightDown);
            Mouse.Button(Mouse.MouseEvents.RightUp);
            return true;

          case RemoteButton_my.Play: // Middle Single-Click
            if (_mouseModeLeftHeld)
              Mouse.Button(Mouse.MouseEvents.LeftUp);

            if (_mouseModeRightHeld)
              Mouse.Button(Mouse.MouseEvents.RightUp);

            if (_mouseModeMiddleHeld)
              Mouse.Button(Mouse.MouseEvents.MiddleUp);

            _mouseModeLeftHeld = false;
            _mouseModeRightHeld = false;
            _mouseModeMiddleHeld = false;

            Mouse.Button(Mouse.MouseEvents.MiddleDown);
            Mouse.Button(Mouse.MouseEvents.MiddleUp);
            return true;

          case RemoteButton_my.Ok: // Double-Click (Left)
            if (_mouseModeLeftHeld)
              Mouse.Button(Mouse.MouseEvents.LeftUp);

            if (_mouseModeRightHeld)
              Mouse.Button(Mouse.MouseEvents.RightUp);

            if (_mouseModeMiddleHeld)
              Mouse.Button(Mouse.MouseEvents.MiddleUp);

            _mouseModeLeftHeld = false;
            _mouseModeRightHeld = false;
            _mouseModeMiddleHeld = false;

            Mouse.Button(Mouse.MouseEvents.LeftDown);
            Mouse.Button(Mouse.MouseEvents.LeftUp);

            Mouse.Button(Mouse.MouseEvents.LeftDown);
            Mouse.Button(Mouse.MouseEvents.LeftUp);
            return true;

          case RemoteButton_my.Back: // Left Click & Hold
            if (_mouseModeRightHeld)
              Mouse.Button(Mouse.MouseEvents.RightUp);

            if (_mouseModeMiddleHeld)
              Mouse.Button(Mouse.MouseEvents.MiddleUp);

            if (_mouseModeLeftHeld)
              Mouse.Button(Mouse.MouseEvents.LeftUp);
            else
              Mouse.Button(Mouse.MouseEvents.LeftDown);

            _mouseModeLeftHeld = !_mouseModeLeftHeld;
            _mouseModeRightHeld = false;
            _mouseModeMiddleHeld = false;
            return true;

          case RemoteButton_my.Info: // Right Click & Hold
            if (_mouseModeLeftHeld)
              Mouse.Button(Mouse.MouseEvents.LeftUp);

            if (_mouseModeMiddleHeld)
              Mouse.Button(Mouse.MouseEvents.MiddleUp);

            if (_mouseModeRightHeld)
              Mouse.Button(Mouse.MouseEvents.RightUp);
            else
              Mouse.Button(Mouse.MouseEvents.RightDown);

            _mouseModeRightHeld = !_mouseModeRightHeld;
            _mouseModeLeftHeld = false;
            _mouseModeMiddleHeld = false;
            return true;

          case RemoteButton_my.Stop: // Middle Click & Hold
            if (_mouseModeLeftHeld)
              Mouse.Button(Mouse.MouseEvents.LeftUp);

            if (_mouseModeRightHeld)
              Mouse.Button(Mouse.MouseEvents.RightUp);

            if (_mouseModeMiddleHeld)
              Mouse.Button(Mouse.MouseEvents.MiddleUp);
            else
              Mouse.Button(Mouse.MouseEvents.MiddleDown);

            _mouseModeMiddleHeld = !_mouseModeMiddleHeld;
            _mouseModeLeftHeld = false;
            _mouseModeRightHeld = false;
            return true;

          case RemoteButton_my.ChannelUp: // Scroll Up
            Mouse.Scroll(Mouse.ScrollDir.Up);
            return true;

          case RemoteButton_my.ChannelDown: // Scroll Down
            Mouse.Scroll(Mouse.ScrollDir.Down);
            return true;
        }
      }

      return false;
    }

    /// <summary>
    /// Handles remote buttons received.
    /// </summary>
    /// <param name="deviceName">Name of the device.</param>
    /// <param name="keyCode">The remote button.</param>
    private static void RemoteHandler(string deviceName, string keyCode)
    {
      // If user has stipulated that MP must have focus to recognize commands ...
      if (RequireFocus && !GUIGraphicsContext.HasFocus)
        return;

      foreach (MappedKeyCode mapping in _remoteMap)
      {
        if (!mapping.KeyCode.Equals(keyCode, StringComparison.OrdinalIgnoreCase))
          continue;

        if (MultiMappingEnabled && mapping.Button == MultiMappingButton)
        {
          ChangeMultiMapping();
          return;
        }

        if (MouseModeEnabled)
          if (HandleMouseMode(mapping.Button))
            return;

        // Get & execute Mapping
        bool gotMapped;
        if (MultiMappingEnabled)
          gotMapped = _multiInputHandlers[_multiMappingSet].MapAction((int) mapping.Button);
        else
          gotMapped = _defaultInputHandler.MapAction((int) mapping.Button);

        if (gotMapped)
          Log.Debug("MPControlPlugin: Command \"{0}\" mapped to remote", mapping.Button);
        else
            Log.Debug("MPControlPlugin: Command \"{0}\" not mapped to remote", mapping.Button);

        return;
      }

      Log.Debug("MPControlPlugin: keyCode \"{0}\" was not handled", keyCode);

      return;
    }

    private static void CommsFailure(object obj)
    {
      Exception ex = obj as Exception;

      if (ex != null)
        Log.Error("MPControlPlugin: Communications failure: {0}", ex.ToString());
      else
        Log.Error("MPControlPlugin: Communications failure");

      StopClient();

      Log.Warn("MPControlPlugin: Attempting communications restart ...");

      IPAddress serverIP = Network.GetIPFromName(_serverHost);
      IPEndPoint endPoint = new IPEndPoint(serverIP, Server.DefaultPort);

      StartClient(endPoint);
    }

    private static void Connected(object obj)
    {
      Log.Info("MPControlPlugin: Connected to server");

      IrssMessage message = new IrssMessage(MessageType.RegisterClient, MessageFlags.Request);
      _client.Send(message);
    }

    private static void Disconnected(object obj)
    {
      Log.Warn("MPControlPlugin: Communications with server has been lost");

      Thread.Sleep(1000);
    }

    internal static bool StartClient(IPEndPoint endPoint)
    {
      if (_client != null)
        return false;

      ClientMessageSink sink = ReceivedMessage;

      _client = new Client(endPoint, sink);
      _client.CommsFailureCallback = CommsFailure;
      _client.ConnectCallback = Connected;
      _client.DisconnectCallback = Disconnected;

      if (_client.Start())
        return true;

      _client = null;
      return false;
    }

    internal static void StopClient()
    {
      if (_client == null)
        return;

      _client.Dispose();
      _client = null;

      _registered = false;
    }

    private static void ReceivedMessage(IrssMessage received)
    {
      Log.Debug("MPControlPlugin: Received Message \"{0}\"", received.Type);

      try
      {
        switch (received.Type)
        {
          case MessageType.RemoteEvent:
            if (!_inConfiguration)
            {
              string deviceName = received.MessageData[IrssMessage.DEVICE_NAME] as string;
              string keyCode = received.MessageData[IrssMessage.KEY_CODE] as string;

              RemoteHandler(deviceName, keyCode);
            }
            break;

          case MessageType.BlastIR:
            if ((received.Flags & MessageFlags.Success) == MessageFlags.Success)
            {
              Log.Debug("MPControlPlugin: Blast successful");
            }
            else if ((received.Flags & MessageFlags.Failure) == MessageFlags.Failure)
            {
              Log.Warn("MPControlPlugin: Failed to blast IR command");
            }
            break;

          case MessageType.RegisterClient:
            if ((received.Flags & MessageFlags.Success) == MessageFlags.Success)
            {
              _irServerInfo = IRServerInfo.FromBytes(received.GetDataAsBytes());
              _registered = true;

              Log.Debug("MPControlPlugin: Registered to IR Server");
            }
            else if ((received.Flags & MessageFlags.Failure) == MessageFlags.Failure)
            {
              _registered = false;
              Log.Warn("MPControlPlugin: IR Server refused to register");
            }
            break;

          case MessageType.LearnIR:
            if ((received.Flags & MessageFlags.Success) == MessageFlags.Success)
            {
              Log.Debug("MPControlPlugin: Learned IR Successfully");

              byte[] dataBytes = received.GetDataAsBytes();

              using (FileStream file = File.Create(_learnIRFilename))
                file.Write(dataBytes, 0, dataBytes.Length);
            }
            else if ((received.Flags & MessageFlags.Failure) == MessageFlags.Failure)
            {
              Log.Error("MPControlPlugin: Failed to learn IR command");
            }
            else if ((received.Flags & MessageFlags.Timeout) == MessageFlags.Timeout)
            {
              Log.Error("MPControlPlugin: Learn IR command timed-out");
            }

            _learnIRFilename = null;
            break;

          case MessageType.ServerShutdown:
            Log.Warn("MPControlPlugin: IR Server Shutdown - Plugin disabled until IR Server returns");
            _registered = false;
            break;

          case MessageType.Error:
            _learnIRFilename = null;
            Log.Error("MPControlPlugin: Received error: {0}", received.GetDataAsString());
            break;
        }

        if (_handleMessage != null)
          _handleMessage(received);
      }
      catch (Exception ex)
      {
        _learnIRFilename = null;
        Log.Error(ex);
      }
    }

    /// <summary>
    /// OnMessage is used for event mapping.
    /// </summary>
    /// <param name="msg">Message.</param>
    private void OnMessage(GUIMessage msg)
    {
      MapEvent(msg);
    }

    /// <summary>
    /// Changes the multi mapping.
    /// </summary>
    private static void ChangeMultiMapping()
    {
      // Cycle through Multi-Mappings ...
      _multiMappingSet = (_multiMappingSet + 1) % MultiMaps.Length;

      // Show the mapping set name on screen ...
      string setName = MultiMaps[_multiMappingSet];

      Log.Debug("MPControlPlugin: Multi-Mapping has cycled to \"{0}\"", setName);

      MPCommon.ShowNotifyDialog("Multi-Mapping", setName, 2);
    }

    /// <summary>
    /// Changes the multi mapping.
    /// </summary>
    /// <param name="multiMapping">The multi mapping.</param>
    private static void ChangeMultiMapping(string multiMapping)
    {
      Log.Debug("MPControlPlugin: ChangeMultiMapping: {0}", multiMapping);

      if (multiMapping.Equals("TOGGLE", StringComparison.OrdinalIgnoreCase))
      {
        ChangeMultiMapping();
        return;
      }

      for (int index = 0; index < MultiMaps.Length; index++)
      {
        if (MultiMaps[index].Equals(multiMapping, StringComparison.CurrentCultureIgnoreCase))
        {
          _multiMappingSet = index;

          // Show the mapping set name on screen ...
          string setName = MultiMaps[_multiMappingSet];

          Log.Debug("MPControlPlugin: Multi-Mapping has changed to \"{0}\"", setName);

          MPCommon.ShowNotifyDialog("Multi-Mapping", setName, 2);
          return;
        }
      }

      Log.Warn("MPControlPlugin: Could not find Multi-Mapping \"{0}\"", multiMapping);
    }

    /// <summary>
    /// Loads the remote map.
    /// </summary>
    /// <param name="remoteFile">The remote file.</param>
    /// <returns>Remote map.</returns>
    private static MappedKeyCode[] LoadRemoteMap(string remoteFile)
    {
      List<MappedKeyCode> remoteMap = new List<MappedKeyCode>();

      Log.Error("MPControlPlugin: Try loading remotemap \"{0}\"", remoteFile);

      try
      {
        XmlDocument doc = new XmlDocument();
        doc.Load(remoteFile);

        XmlNodeList listRemotes = doc.DocumentElement.SelectNodes("remote");
        foreach (XmlNode nodeRemote in listRemotes)
        {
          string remoteName = nodeRemote.Attributes["name"].Value;

          XmlNodeList listButtons = nodeRemote.SelectNodes("button");
          foreach (XmlNode nodeButton in listButtons)
          {
            string remoteButton = nodeButton.Attributes["name"].Value;

            XmlNodeList listIRCodes = nodeButton.SelectNodes("code");
            foreach (XmlNode nodeCode in listIRCodes)
            {
              string remoteCode = nodeCode.Attributes["value"].Value;
                try
                {
                    MappedKeyCode keycode = new MappedKeyCode(remoteButton, remoteCode);
                    remoteMap.Add(keycode);
                }
                catch (Exception ex)
                {
                    Log.Error("MPControlPlugin - MappedKeyCode(): {0}", ex.ToString());
                }
            }
          }
        }
      }
      catch (Exception ex)
      {
        Log.Error("MPControlPlugin - LoadRemoteMap(): {0}", ex.ToString());
      }

      return remoteMap.ToArray();
    }

    /// <summary>
    /// Loads the event mappings.
    /// </summary>
    private static void LoadEventMappings()
    {
      try
      {
        XmlDocument doc = new XmlDocument();
        doc.Load(EventMappingFile);

        XmlNodeList eventList = doc.DocumentElement.SelectNodes("mapping");

        _eventMappings = new List<MappedEvent>(eventList.Count);

        foreach (XmlNode item in eventList)
          EventMappings.Add(MappedEvent.FromStrings(item.Attributes["event"].Value, item.Attributes["command"].Value));
      }
      catch (Exception ex)
      {
        Log.Error(ex);
      }
    }

    /// <summary>
    /// Run the event mapper over the supplied GUIMessage.
    /// </summary>
    /// <param name="msg">MediaPortal Message to run through the event mapper.</param>
    private static void MapEvent(GUIMessage msg)
    {
      MappedEvent.MappingEvent eventType = MappedEvent.GetEventType(msg.Message);
      if (eventType == MappedEvent.MappingEvent.None)
        return;

      foreach (MappedEvent mappedEvent in EventMappings)
      {
        if (mappedEvent.EventType == eventType)
        {
          bool matched = true;

          if (mappedEvent.MatchParam)
          {
            string paramValueString = mappedEvent.ParamValue;
            int paramValueInt = -1;
            int.TryParse(mappedEvent.ParamValue, out paramValueInt);
            bool paramValueBool = false;
            bool.TryParse(mappedEvent.ParamValue, out paramValueBool);

            switch (mappedEvent.Param)
            {
              case "Label 1":
                matched = (msg.Label.Equals(paramValueString, StringComparison.OrdinalIgnoreCase));
                break;
              case "Label 2":
                matched = (msg.Label2.Equals(paramValueString, StringComparison.OrdinalIgnoreCase));
                break;
              case "Label 3":
                matched = (msg.Label3.Equals(paramValueString, StringComparison.OrdinalIgnoreCase));
                break;
              case "Label 4":
                matched = (msg.Label4.Equals(paramValueString, StringComparison.OrdinalIgnoreCase));
                break;
              case "Parameter 1":
                matched = (msg.Param1 == paramValueInt);
                break;
              case "Parameter 2":
                matched = (msg.Param2 == paramValueInt);
                break;
              case "Parameter 3":
                matched = (msg.Param3 == paramValueInt);
                break;
              case "Parameter 4":
                matched = (msg.Param4 == paramValueInt);
                break;
              case "Sender Control ID":
                matched = (msg.SenderControlId == paramValueInt);
                break;
              case "Send To Target Window":
                matched = (msg.SendToTargetWindow == paramValueBool);
                break;
              case "Target Control ID":
                matched = (msg.TargetControlId == paramValueInt);
                break;
              case "Target Window ID":
                matched = (msg.TargetWindowId == paramValueInt);
                break;
              default:
                matched = false;
                break;
            }
          }

          if (!matched)
            continue;

          Log.Debug("MPControlPlugin: Event Mapper - Event \"{0}\"",
                    Enum.GetName(typeof (MappedEvent.MappingEvent), eventType));

          try
          {
            ProcessCommand(mappedEvent.Command, false);
          }
          catch (Exception ex)
          {
            Log.Error("MPControlPlugin: Failed to execute Event Mapper command \"{0}\" - {1}", mappedEvent.EventType,
                      ex.ToString());
          }
        }
      }
    }

    /// <summary>
    /// Run the event mapper over the supplied MappedEvent type.
    /// </summary>
    /// <param name="eventType">MappedEvent to run through the event mapper.</param>
    private static void MapEvent(MappedEvent.MappingEvent eventType)
    {
      foreach (MappedEvent mappedEvent in EventMappings)
      {
        if (mappedEvent.EventType == eventType)
        {
          if (mappedEvent.MatchParam)
            continue;

          Log.Debug("MPControlPlugin: Event Mapper - Event \"{0}\"",
                    Enum.GetName(typeof (MappedEvent.MappingEvent), eventType));

          try
          {
            ProcessCommand(mappedEvent.Command, false);
          }
          catch (Exception ex)
          {
            Log.Error("MPControlPlugin: Failed to execute Event Mapper command \"{0}\" - {1}", mappedEvent.EventType,
                      ex.ToString());
          }
        }
      }
    }

    /// <summary>
    /// Handles the PowerModeChanged event of the SystemEvents control.
    /// </summary>
    /// <param name="sender">The source of the event.</param>
    /// <param name="e">The <see cref="Microsoft.Win32.PowerModeChangedEventArgs"/> instance containing the event data.</param>
    private static void SystemEvents_PowerModeChanged(object sender, PowerModeChangedEventArgs e)
    {
      switch (e.Mode)
      {
        case PowerModes.Resume:
          OnResume();
          break;

        case PowerModes.Suspend:
          OnSuspend();
          break;
      }
    }

    /// <summary>
    /// Loads the default remote button input handler.
    /// </summary>
    internal static void LoadDefaultMapping()
    {
      _defaultInputHandler = new InputHandler("MPControlPlugin");

      if (!_defaultInputHandler.IsLoaded)
        Log.Error("MPControlPlugin: Error loading default mapping file");
    }

    /// <summary>
    /// Loads multi-mappings for input handling.
    /// </summary>
    internal static void LoadMultiMappings()
    {
      XmlDocument doc = new XmlDocument();
      doc.Load(MultiMappingFile);

      XmlNodeList mappings = doc.DocumentElement.SelectNodes("map");

      _multiInputHandlers = new List<InputHandler>(mappings.Count);
      _multiMaps = new string[mappings.Count];

      for (int index = 0; index < mappings.Count; index++)
      {
        string map = mappings.Item(index).Attributes["name"].Value;

        _multiMaps[index] = map;
        _multiInputHandlers.Add(new InputHandler(map));

        if (!_multiInputHandlers[index].IsLoaded)
          Log.Error("MPControlPlugin: Error loading default mapping file for {0}", map);
      }
    }

    /// <summary>
    /// Call this when entering standby to ensure that the Event Mapper is informed.
    /// </summary>
    internal static void OnSuspend()
    {
      if (!_inConfiguration && EventMapperEnabled)
        MapEvent(MappedEvent.MappingEvent.PC_Suspend);
    }

    /// <summary>
    /// Call this when leaving standby to ensure the Event Mapper is informed.
    /// </summary>
    internal static void OnResume()
    {
      if (!_inConfiguration && EventMapperEnabled)
        MapEvent(MappedEvent.MappingEvent.PC_Resume);
    }

    /// <summary>
    /// Learn an IR command.
    /// </summary>
    /// <param name="fileName">File to place learned IR command in (absolute path).</param>
    /// <returns><c>true</c> if successful, otherwise <c>false</c>.</returns>
    internal static bool LearnIR(string fileName)
    {
      try
      {
        if (String.IsNullOrEmpty(fileName))
        {
          Log.Error("MPControlPlugin: Null or Empty file name for LearnIR()");
          return false;
        }

        if (!_registered)
        {
          Log.Warn("MPControlPlugin: Not registered to an active IR Server");
          return false;
        }

        if (_learnIRFilename != null)
        {
          Log.Warn("MPControlPlugin: Already trying to learn an IR command");
          return false;
        }

        _learnIRFilename = fileName;

        IrssMessage message = new IrssMessage(MessageType.LearnIR, MessageFlags.Request);
        _client.Send(message);
      }
      catch (Exception ex)
      {
        _learnIRFilename = null;
        Log.Error(ex);
        return false;
      }

      return true;
    }

    /// <summary>
    /// Blast an IR command.
    /// </summary>
    /// <param name="fileName">File to blast (absolute path).</param>
    /// <param name="port">Port to blast to.</param>
    internal static void BlastIR(string fileName, string port)
    {
      Log.Debug("MPControlPlugin: MPControlPlugin - BlastIR(): {0}, {1}", fileName, port);

      if (!_registered)
        throw new InvalidOperationException("Cannot Blast, not registered to an active IR Server");

      using (FileStream file = File.OpenRead(fileName))
      {
        if (file.Length == 0)
          throw new IOException(String.Format("Cannot Blast. IR file \"{0}\" has no data, possible IR learn failure",
                                              fileName));

        byte[] outData = new byte[4 + port.Length + file.Length];

        BitConverter.GetBytes(port.Length).CopyTo(outData, 0);
        Encoding.ASCII.GetBytes(port).CopyTo(outData, 4);

        file.Read(outData, 4 + port.Length, (int) file.Length);

        IrssMessage message = new IrssMessage(MessageType.BlastIR, MessageFlags.Request, outData);
        _client.Send(message);
      }
    }

    /// <summary>
    /// Given a command this method processes the request accordingly.
    /// Throws exceptions only if run synchronously, if async exceptions are logged instead.
    /// </summary>
    /// <param name="command">Command to process.</param>
    /// <param name="async">Process command asynchronously?</param>
    internal static void ProcessCommand(string command, bool async)
    {
      if (async)
      {
        try
        {
          Thread newThread = new Thread(ProcCommand);
          newThread.Name = ProcessCommandThreadName;
          newThread.IsBackground = true;
          newThread.Start(command);
        }
        catch (Exception ex)
        {
          Log.Error(ex);
        }
      }
      else
      {
        ProcCommand(command);
      }
    }

    /// <summary>
    /// Used by ProcessCommand to actually handle the command.
    /// Can be called Synchronously or as a Parameterized Thread.
    /// </summary>
    /// <param name="commandObj">Command string to process.</param>
    private static void ProcCommand(object commandObj)
    {
      try
      {
        if (commandObj == null)
          throw new ArgumentNullException("commandObj");

        string command = commandObj as string;

        if (String.IsNullOrEmpty(command))
          throw new ArgumentException("commandObj translates to empty or null string", "commandObj");

        if (command.StartsWith(IrssUtils.Common.CmdPrefixMacro, StringComparison.OrdinalIgnoreCase))
        {
          string fileName = Path.Combine(FolderMacros,
                                         command.Substring(IrssUtils.Common.CmdPrefixMacro.Length) + IrssUtils.Common.FileExtensionMacro);
          ProcMacro(fileName);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixBlast, StringComparison.OrdinalIgnoreCase))
        {
          string[] commands = IrssUtils.Common.SplitBlastCommand(command.Substring(IrssUtils.Common.CmdPrefixBlast.Length));
          BlastIR(Path.Combine(IrssUtils.Common.FolderIRCommands, commands[0] + IrssUtils.Common.FileExtensionIR), commands[1]);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixPause, StringComparison.OrdinalIgnoreCase))
        {
          int pauseTime = int.Parse(command.Substring(IrssUtils.Common.CmdPrefixPause.Length));
          Thread.Sleep(pauseTime);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixRun, StringComparison.OrdinalIgnoreCase))
        {
          string[] commands = IrssUtils.Common.SplitRunCommand(command.Substring(IrssUtils.Common.CmdPrefixRun.Length));
          IrssUtils.Common.ProcessRunCommand(commands);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixSerial, StringComparison.OrdinalIgnoreCase))
        {
          string[] commands = IrssUtils.Common.SplitSerialCommand(command.Substring(IrssUtils.Common.CmdPrefixSerial.Length));
          IrssUtils.Common.ProcessSerialCommand(commands);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixWindowMsg, StringComparison.OrdinalIgnoreCase))
        {
          string[] commands = IrssUtils.Common.SplitWindowMessageCommand(command.Substring(IrssUtils.Common.CmdPrefixWindowMsg.Length));
          IrssUtils.Common.ProcessWindowMessageCommand(commands);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixTcpMsg, StringComparison.OrdinalIgnoreCase))
        {
          string[] commands = IrssUtils.Common.SplitTcpMessageCommand(command.Substring(IrssUtils.Common.CmdPrefixTcpMsg.Length));
          IrssUtils.Common.ProcessTcpMessageCommand(commands);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixHttpMsg, StringComparison.OrdinalIgnoreCase))
        {
          string[] commands = IrssUtils.Common.SplitHttpMessageCommand(command.Substring(IrssUtils.Common.CmdPrefixHttpMsg.Length));
          IrssUtils.Common.ProcessHttpCommand(commands);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixKeys, StringComparison.OrdinalIgnoreCase))
        {
          string keyCommand = command.Substring(IrssUtils.Common.CmdPrefixKeys.Length);
          if (_inConfiguration)
            MessageBox.Show(keyCommand, IrssUtils.Common.UITextKeys, MessageBoxButtons.OK, MessageBoxIcon.Information);
          else
            IrssUtils.Common.ProcessKeyCommand(keyCommand);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixMouse, StringComparison.OrdinalIgnoreCase))
        {
          string mouseCommand = command.Substring(IrssUtils.Common.CmdPrefixMouse.Length);
          IrssUtils.Common.ProcessMouseCommand(mouseCommand);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixEject, StringComparison.OrdinalIgnoreCase))
        {
          string ejectCommand = command.Substring(IrssUtils.Common.CmdPrefixEject.Length);
          IrssUtils.Common.ProcessEjectCommand(ejectCommand);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixMultiMap, StringComparison.OrdinalIgnoreCase))
        {
          string multiMapping = command.Substring(IrssUtils.Common.CmdPrefixMultiMap.Length);
          if (_inConfiguration)
            MessageBox.Show(multiMapping, IrssUtils.Common.UITextMultiMap, MessageBoxButtons.OK, MessageBoxIcon.Information);
          else
            ChangeMultiMapping(multiMapping);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixPopup, StringComparison.OrdinalIgnoreCase))
        {
          string[] commands = IrssUtils.Common.SplitPopupCommand(command.Substring(IrssUtils.Common.CmdPrefixPopup.Length));
          if (_inConfiguration)
            MessageBox.Show(commands[1], commands[0], MessageBoxButtons.OK, MessageBoxIcon.Information);
          else
            MPCommon.ShowNotifyDialog(commands[0], commands[1], int.Parse(commands[2]));
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixGotoScreen, StringComparison.OrdinalIgnoreCase))
        {
          string screenID = command.Substring(IrssUtils.Common.CmdPrefixGotoScreen.Length);

          if (_inConfiguration)
            MessageBox.Show(screenID, IrssUtils.Common.UITextGotoScreen, MessageBoxButtons.OK, MessageBoxIcon.Information);
          else
            MPCommon.ProcessGoTo(screenID, _mpBasicHome);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixSendMPAction, StringComparison.OrdinalIgnoreCase))
        {
          string[] commands = IrssUtils.Common.SplitSendMPActionCommand(command.Substring(IrssUtils.Common.CmdPrefixSendMPAction.Length));
          if (_inConfiguration)
            MessageBox.Show(commands[0], IrssUtils.Common.UITextSendMPAction, MessageBoxButtons.OK, MessageBoxIcon.Information);
          else
            MPCommon.ProcessSendMediaPortalAction(commands);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixSendMPMsg, StringComparison.OrdinalIgnoreCase))
        {
          string[] commands = IrssUtils.Common.SplitSendMPMsgCommand(command.Substring(IrssUtils.Common.CmdPrefixSendMPMsg.Length));
          if (_inConfiguration)
            MessageBox.Show(commands[0], IrssUtils.Common.UITextSendMPMsg, MessageBoxButtons.OK, MessageBoxIcon.Information);
          else
            MPCommon.ProcessSendMediaPortalMessage(commands);
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixInputLayer, StringComparison.OrdinalIgnoreCase))
        {
          if (_inConfiguration)
          {
            MessageBox.Show("Cannot toggle the input handler layer while in configuration", IrssUtils.Common.UITextInputLayer,
                            MessageBoxButtons.OK, MessageBoxIcon.Information);
          }
          else
          {
            InputHandler inputHandler;

            if (_multiMappingEnabled)
              inputHandler = _multiInputHandlers[_multiMappingSet];
            else
              inputHandler = _defaultInputHandler;

            if (inputHandler.CurrentLayer == 1)
              inputHandler.CurrentLayer = 2;
            else
              inputHandler.CurrentLayer = 1;
          }
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixExit, StringComparison.OrdinalIgnoreCase))
        {
          if (_inConfiguration)
            MessageBox.Show("Cannot exit MediaPortal in configuration", IrssUtils.Common.UITextExit, MessageBoxButtons.OK,
                            MessageBoxIcon.Information);
          else
            MPCommon.ExitMP();
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixHibernate, StringComparison.OrdinalIgnoreCase))
        {
          if (_inConfiguration)
            MessageBox.Show("Cannot Hibernate in configuration", IrssUtils.Common.UITextHibernate, MessageBoxButtons.OK,
                            MessageBoxIcon.Information);
          else
            MPCommon.Hibernate();
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixReboot, StringComparison.OrdinalIgnoreCase))
        {
          if (_inConfiguration)
            MessageBox.Show("Cannot Reboot in configuration", IrssUtils.Common.UITextReboot, MessageBoxButtons.OK,
                            MessageBoxIcon.Information);
          else
            MPCommon.Reboot();
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixShutdown, StringComparison.OrdinalIgnoreCase))
        {
          if (_inConfiguration)
            MessageBox.Show("Cannot Shutdown in configuration", IrssUtils.Common.UITextShutdown, MessageBoxButtons.OK,
                            MessageBoxIcon.Information);
          else
            MPCommon.ShutDown();
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixStandby, StringComparison.OrdinalIgnoreCase))
        {
          if (_inConfiguration)
            MessageBox.Show("Cannot enter Standby in configuration", IrssUtils.Common.UITextStandby, MessageBoxButtons.OK,
                            MessageBoxIcon.Information);
          else
            MPCommon.Standby();
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixVirtualKB, StringComparison.OrdinalIgnoreCase))
        {
          if (_inConfiguration)
          {
            MessageBox.Show("Cannot show Virtual Keyboard in configuration", IrssUtils.Common.UITextVirtualKB,
                            MessageBoxButtons.OK, MessageBoxIcon.Information);
          }
          else
          {
            VirtualKeyboard vk = new VirtualKeyboard();
            if (vk.ShowDialog() == DialogResult.OK)
              Keyboard.ProcessCommand(vk.TextOutput);
          }
        }
        else if (command.StartsWith(IrssUtils.Common.CmdPrefixSmsKB, StringComparison.OrdinalIgnoreCase))
        {
          if (_inConfiguration)
          {
            MessageBox.Show("Cannot show SMS Keyboard in configuration", IrssUtils.Common.UITextSmsKB, MessageBoxButtons.OK,
                            MessageBoxIcon.Information);
          }
          else
          {
            SmsKeyboard sms = new SmsKeyboard();
            if (sms.ShowDialog() == DialogResult.OK)
              Keyboard.ProcessCommand(sms.TextOutput);
          }
        }
        else
        {
          throw new ArgumentException(String.Format("Cannot process unrecognized command \"{0}\"", command),
                                      "commandObj");
        }
      }
      catch (Exception ex)
      {
        if (!String.IsNullOrEmpty(Thread.CurrentThread.Name) &&
            Thread.CurrentThread.Name.Equals(ProcessCommandThreadName, StringComparison.OrdinalIgnoreCase))
          Log.Error(ex);
        else
          throw;
      }
    }

    /// <summary>
    /// Called by ProcCommand to process the supplied Macro file.
    /// </summary>
    /// <param name="fileName">Macro file to process (absolute path).</param>
    private static void ProcMacro(string fileName)
    {
      XmlDocument doc = new XmlDocument();
      doc.Load(fileName);

      XmlNodeList commandSequence = doc.DocumentElement.SelectNodes("item");

      foreach (XmlNode item in commandSequence)
        ProcCommand(item.Attributes["command"].Value);
    }

    /// <summary>
    /// Returns a list of Macros.
    /// </summary>
    /// <param name="commandPrefix">Add the command prefix to each list item.</param>
    /// <returns>string[] of Macros.</returns>
    internal static string[] GetMacroList(bool commandPrefix)
    {
      string[] files = Directory.GetFiles(FolderMacros, '*' + IrssUtils.Common.FileExtensionMacro);
      string[] list = new string[files.Length];

      int i = 0;
      foreach (string file in files)
      {
        if (commandPrefix)
          list[i++] = IrssUtils.Common.CmdPrefixMacro + Path.GetFileNameWithoutExtension(file);
        else
          list[i++] = Path.GetFileNameWithoutExtension(file);
      }

      return list;
    }

    /// <summary>
    /// Returns a combined list of IR Commands and Macros.
    /// </summary>
    /// <param name="commandPrefix">Add the command prefix to each list item.</param>
    /// <returns>string[] of IR Commands and Macros.</returns>
    internal static string[] GetFileList(bool commandPrefix)
    {
      string[] MacroFiles = Directory.GetFiles(FolderMacros, '*' + IrssUtils.Common.FileExtensionMacro);
      string[] IRFiles = Directory.GetFiles(IrssUtils.Common.FolderIRCommands, '*' + IrssUtils.Common.FileExtensionIR);
      string[] list = new string[MacroFiles.Length + IRFiles.Length];

      int i = 0;
      foreach (string file in MacroFiles)
      {
        if (commandPrefix)
          list[i++] = IrssUtils.Common.CmdPrefixMacro + Path.GetFileNameWithoutExtension(file);
        else
          list[i++] = Path.GetFileNameWithoutExtension(file);
      }

      foreach (string file in IRFiles)
      {
        if (commandPrefix)
          list[i++] = IrssUtils.Common.CmdPrefixBlast + Path.GetFileNameWithoutExtension(file);
        else
          list[i++] = Path.GetFileNameWithoutExtension(file);
      }

      return list;
    }

    /// <summary>
    /// Loads the settings.
    /// </summary>
    private static void LoadSettings()
    {
      try
      {
        using (Settings xmlreader = new Settings(MPCommon.MPConfigFile))
        {
          ServerHost = xmlreader.GetValueAsString("MPControlPlugin", "ServerHost", "localhost");

          RequireFocus = xmlreader.GetValueAsBool("MPControlPlugin", "RequireFocus", true);
          MultiMappingEnabled = xmlreader.GetValueAsBool("MPControlPlugin", "MultiMappingEnabled", false);
          MultiMappingButton =
            (RemoteButton_my) xmlreader.GetValueAsInt("MPControlPlugin", "MultiMappingButton", (int) RemoteButton_my.Start);
          EventMapperEnabled = xmlreader.GetValueAsBool("MPControlPlugin", "EventMapperEnabled", false);
          MouseModeButton =
            (RemoteButton_my) xmlreader.GetValueAsInt("MPControlPlugin", "MouseModeButton", (int) RemoteButton_my.Teletext);
          MouseModeEnabled = xmlreader.GetValueAsBool("MPControlPlugin", "MouseModeEnabled", false);
          MouseModeStep = xmlreader.GetValueAsInt("MPControlPlugin", "MouseModeStep", 10);
          MouseModeAcceleration = xmlreader.GetValueAsBool("MPControlPlugin", "MouseModeAcceleration", true);

          // MediaPortal settings ...
          _mpBasicHome = xmlreader.GetValueAsBool("general", "startbasichome", false);
        }
      }
      catch (Exception ex)
      {
        Log.Error(ex);
      }
    }

    /// <summary>
    /// Saves the settings.
    /// </summary>
    private static void SaveSettings()
    {
      try
      {
        using (Settings xmlwriter = new Settings(MPCommon.MPConfigFile))
        {
          xmlwriter.SetValue("MPControlPlugin", "ServerHost", ServerHost);

          xmlwriter.SetValueAsBool("MPControlPlugin", "RequireFocus", RequireFocus);
          xmlwriter.SetValueAsBool("MPControlPlugin", "MultiMappingEnabled", MultiMappingEnabled);
          xmlwriter.SetValue("MPControlPlugin", "MultiMappingButton", (int) MultiMappingButton);
          xmlwriter.SetValueAsBool("MPControlPlugin", "EventMapperEnabled", EventMapperEnabled);
          xmlwriter.SetValue("MPControlPlugin", "MouseModeButton", (int) MouseModeButton);
          xmlwriter.SetValueAsBool("MPControlPlugin", "MouseModeEnabled", MouseModeEnabled);
          xmlwriter.SetValue("MPControlPlugin", "MouseModeStep", MouseModeStep);
          xmlwriter.SetValueAsBool("MPControlPlugin", "MouseModeAcceleration", MouseModeAcceleration);
        }
      }
      catch (Exception ex)
      {
        Log.Error(ex);
      }
    }

    #endregion Implementation
  }

  #region receiverwindow

  #region Delegates

  /// <summary>
  /// Windows message processing delegate.
  /// </summary>
  /// <param name="m">Windows message.</param>
  internal delegate void ProcessMessage(ref Message m);

  #endregion Delegates

  /// <summary>
  /// Use this class to receive windows messages.
  /// </summary>
  internal class ReceiverWindow : NativeWindow
  {
      #region Variables

      private ProcessMessage _processMessage;

      #endregion Variables

      #region Properties

      /// <summary>
      /// Gets or Sets the Windows Message processing delegate.
      /// </summary>
      public ProcessMessage ProcMsg
      {
          get { return _processMessage; }
          set { _processMessage = value; }
      }

      #endregion Properties

      #region Constructor/Destructor

      /// <summary>
      /// Create a Windows Message receiving window object.
      /// </summary>
      /// <param name="windowTitle">Window title for receiver object.</param>
      public ReceiverWindow(string windowTitle)
      {
          CreateParams createParams = new CreateParams();
          createParams.Caption = windowTitle;
          createParams.ExStyle = 0x80;
          createParams.Style = unchecked((int)0x80000000);

          CreateHandle(createParams);
      }

      ~ReceiverWindow()
      {
          if (Handle != IntPtr.Zero)
              DestroyHandle();
      }

      #endregion Constructor/Destructor

      #region Implementation

      protected override void WndProc(ref Message m)
      {
          if (_processMessage != null)
              _processMessage(ref m);

          if (m.Result == (IntPtr)0)
              base.WndProc(ref m);
      }

      #endregion Implementation
  }
  #endregion

  internal class DeviceDetails
  {
      private string _id;
      private string _name;
      private ushort _usage;
      private ushort _usagePage;

      /// <summary>
      /// Gets or sets the name.
      /// </summary>
      /// <value>The name.</value>
      public string Name
      {
          get { return _name; }
          set { _name = value; }
      }

      /// <summary>
      /// Gets or sets the ID.
      /// </summary>
      /// <value>The ID.</value>
      public string ID
      {
          get { return _id; }
          set { _id = value; }
      }

      /// <summary>
      /// Gets or sets the usage page.
      /// </summary>
      /// <value>The usage page.</value>
      public ushort UsagePage
      {
          get { return _usagePage; }
          set { _usagePage = value; }
      }

      /// <summary>
      /// Gets or sets the usage.
      /// </summary>
      /// <value>The usage.</value>
      public ushort Usage
      {
          get { return _usage; }
          set { _usage = value; }
      }
  }

  internal static class RawInput
  {
      #region Interop

      [DllImport("User32.dll")]
      internal static extern uint GetRawInputData(
        IntPtr hRawInput,
        RawInputCommand uiCommand,
        IntPtr pData,
        ref uint pcbSize,
        uint cbSizeHeader);

      [DllImport("User32.dll")]
      internal static extern bool RegisterRawInputDevices(
        RAWINPUTDEVICE[] pRawInputDevice,
        uint uiNumDevices,
        uint cbSize);

      [DllImport("User32.dll")]
      internal static extern uint GetRawInputDeviceList(
        IntPtr pRawInputDeviceList,
        ref uint uiNumDevices,
        uint cbSize);

      [DllImport("User32.dll")]
      internal static extern uint GetRawInputDeviceInfo(
        IntPtr hDevice,
        uint uiCommand,
        IntPtr pData,
        ref uint pcbSize);

      [DllImport("User32.dll")]
      internal static extern uint GetRawInputDeviceInfo(
        IntPtr deviceHandle,
        uint uiCommand,
        ref DeviceInfo data,
        ref uint dataSize);

      #endregion Interop

      #region Constants

      public const int KEYBOARD_OVERRUN_MAKE_CODE = 0x00FF;
      public const int RIDI_DEVICEINFO = 0x2000000B;
      public const int RIDI_DEVICENAME = 0x20000007;
      public const int RIDI_PREPARSEDDATA = 0x20000005;
      public const int WM_APPCOMMAND = 0x0319;
      public const int WM_INPUT = 0x00FF;

      #endregion Constants

      #region Enumerations

      #region RawInputCommand enum

      public enum RawInputCommand
      {
          Input = 0x10000003,
          Header = 0x10000005,
      }

      #endregion

      #region RawInputDeviceFlags enum

      [Flags]
      public enum RawInputDeviceFlags
      {
          /// <summary>No flags.</summary>
          None = 0,
          /// <summary>If set, this removes the top level collection from the inclusion list. This tells the operating system to stop reading from a device which matches the top level collection.</summary>
          Remove = 0x00000001,
          /// <summary>If set, this specifies the top level collections to exclude when reading a complete usage page. This flag only affects a TLC whose usage page is already specified with PageOnly.</summary>
          Exclude = 0x00000010,
          /// <summary>If set, this specifies all devices whose top level collection is from the specified usUsagePage. Note that Usage must be zero. To exclude a particular top level collection, use Exclude.</summary>
          PageOnly = 0x00000020,
          /// <summary>If set, this prevents any devices specified by UsagePage or Usage from generating legacy messages. This is only for the mouse and keyboard.</summary>
          NoLegacy = 0x00000030,
          /// <summary>If set, this enables the caller to receive the input even when the caller is not in the foreground. Note that WindowHandle must be specified.</summary>
          InputSink = 0x00000100,
          /// <summary>If set, the mouse button click does not activate the other window.</summary>
          CaptureMouse = 0x00000200,
          /// <summary>If set, the application-defined keyboard device hotkeys are not handled. However, the system hotkeys; for example, ALT+TAB and CTRL+ALT+DEL, are still handled. By default, all keyboard hotkeys are handled. NoHotKeys can be specified even if NoLegacy is not specified and WindowHandle is NULL.</summary>
          NoHotKeys = 0x00000200,
          /// <summary>If set, application keys are handled.  NoLegacy must be specified.  Keyboard only.</summary>
          AppKeys = 0x00000400
      }

      #endregion

      #region RawInputType enum

      public enum RawInputType
      {
          Mouse = 0,
          Keyboard = 1,
          HID = 2
      }

      #endregion

      #region RawKeyboardFlags enum

      [Flags]
      public enum RawKeyboardFlags : ushort
      {
          KeyMake = 0x00,
          KeyBreak = 0x01,
          KeyE0 = 0x02,
          KeyE1 = 0x04,
          TerminalServerSetLED = 0x08,
          TerminalServerShadow = 0x10
      }

      #endregion

      #region RawMouseButtons enum

      [Flags]
      public enum RawMouseButtons : ushort
      {
          None = 0,
          LeftDown = 0x0001,
          LeftUp = 0x0002,
          RightDown = 0x0004,
          RightUp = 0x0008,
          MiddleDown = 0x0010,
          MiddleUp = 0x0020,
          Button4Down = 0x0040,
          Button4Up = 0x0080,
          Button5Down = 0x0100,
          Button5Up = 0x0200,
          MouseWheel = 0x0400
      }

      #endregion

      #region RawMouseFlags enum

      [Flags]
      public enum RawMouseFlags : ushort
      {
          MoveRelative = 0,
          MoveAbsolute = 1,
          VirtualDesktop = 2,
          AttributesChanged = 4
      }

      #endregion

      #endregion Enumerations

      #region Structures

      #region Nested type: BUTTONSSTR

      [StructLayout(LayoutKind.Sequential)]
      public struct BUTTONSSTR
      {
          [MarshalAs(UnmanagedType.U2)]
          public ushort usButtonFlags;
          [MarshalAs(UnmanagedType.U2)]
          public ushort usButtonData;
      }

      #endregion

      #region Nested type: DeviceInfo

      [StructLayout(LayoutKind.Explicit)]
      public struct DeviceInfo
      {
          [FieldOffset(0)]
          public int Size;
          [FieldOffset(4)]
          public RawInputType Type;

          [FieldOffset(8)]
          public DeviceInfoMouse MouseInfo;
          [FieldOffset(8)]
          public DeviceInfoKeyboard KeyboardInfo;
          [FieldOffset(8)]
          public DeviceInfoHID HIDInfo;
      }

      #endregion

      #region Nested type: DeviceInfoHID

      public struct DeviceInfoHID
      {
          public uint VendorID;
          public uint ProductID;
          public uint Revision;
          public ushort UsagePage;
          public ushort Usage;
          public uint VersionNumber;
      }

      #endregion

      #region Nested type: DeviceInfoKeyboard

      public struct DeviceInfoKeyboard
      {
          public uint KeyboardMode;
          public uint NumberOfFunctionKeys;
          public uint NumberOfIndicators;
          public uint NumberOfKeysTotal;
          public uint SubType;
          public uint Type;
      }

      #endregion

      #region Nested type: DeviceInfoMouse

      public struct DeviceInfoMouse
      {
          public uint ID;
          public uint NumberOfButtons;
          public uint SampleRate;
      }

      #endregion

      #region Nested type: RAWHID

      [StructLayout(LayoutKind.Sequential)]
      public struct RAWHID
      {
          [MarshalAs(UnmanagedType.U4)]
          public int dwSizeHid;
          [MarshalAs(UnmanagedType.U4)]
          public int dwCount;

          //public IntPtr bRawData;
      }

      #endregion

      #region Nested type: RAWINPUT

      [StructLayout(LayoutKind.Explicit)]
      public struct RAWINPUT
      {
          [FieldOffset(0)]
          public RAWINPUTHEADER header;
          [FieldOffset(16)]
          public RAWMOUSE mouse;
          [FieldOffset(16)]
          public RAWKEYBOARD keyboard;
          [FieldOffset(16)]
          public RAWHID hid;
      }

      #endregion

      #region Nested type: RAWINPUTDEVICE

      [StructLayout(LayoutKind.Sequential)]
      public struct RAWINPUTDEVICE
      {
          [MarshalAs(UnmanagedType.U2)]
          public ushort usUsagePage;
          [MarshalAs(UnmanagedType.U2)]
          public ushort usUsage;
          [MarshalAs(UnmanagedType.U4)]
          public RawInputDeviceFlags dwFlags;
          public IntPtr hwndTarget;
      }

      #endregion

      #region Nested type: RAWINPUTDEVICELIST

      [StructLayout(LayoutKind.Sequential)]
      public struct RAWINPUTDEVICELIST
      {
          public IntPtr hDevice;
          [MarshalAs(UnmanagedType.U4)]
          public RawInputType dwType;
      }

      #endregion

      #region Nested type: RAWINPUTHEADER

      [StructLayout(LayoutKind.Sequential)]
      public struct RAWINPUTHEADER
      {
          [MarshalAs(UnmanagedType.U4)]
          public RawInputType dwType;
          [MarshalAs(UnmanagedType.U4)]
          public int dwSize;
          public IntPtr hDevice;
          [MarshalAs(UnmanagedType.U4)]
          public int wParam;
      }

      #endregion

      #region Nested type: RAWKEYBOARD

      [StructLayout(LayoutKind.Sequential)]
      public struct RAWKEYBOARD
      {
          [MarshalAs(UnmanagedType.U2)]
          public ushort MakeCode;
          [MarshalAs(UnmanagedType.U2)]
          public RawKeyboardFlags Flags;
          [MarshalAs(UnmanagedType.U2)]
          public ushort Reserved;
          [MarshalAs(UnmanagedType.U2)]
          public ushort VKey;
          [MarshalAs(UnmanagedType.U4)]
          public uint Message;
          [MarshalAs(UnmanagedType.U4)]
          public uint ExtraInformation;
      }

      #endregion

      #region Nested type: RAWMOUSE

      [StructLayout(LayoutKind.Explicit)]
      public struct RAWMOUSE
      {
          [MarshalAs(UnmanagedType.U2)]
          [FieldOffset(0)]
          public ushort usFlags;
          [MarshalAs(UnmanagedType.U4)]
          [FieldOffset(4)]
          public uint ulButtons;
          [FieldOffset(4)]
          public BUTTONSSTR buttonsStr;
          [MarshalAs(UnmanagedType.U4)]
          [FieldOffset(8)]
          public uint ulRawButtons;
          [FieldOffset(12)]
          public int lLastX;
          [FieldOffset(16)]
          public int lLastY;
          [MarshalAs(UnmanagedType.U4)]
          [FieldOffset(20)]
          public uint ulExtraInformation;
      }

      #endregion

      #endregion Structures

      public static List<DeviceDetails> EnumerateDevices()
      {
          uint deviceCount = 0;
          int dwSize = Marshal.SizeOf(typeof(RAWINPUTDEVICELIST));

          // Get the number of raw input devices in the list,
          // then allocate sufficient memory and get the entire list
          if (GetRawInputDeviceList(IntPtr.Zero, ref deviceCount, (uint)dwSize) == 0)
          {
              IntPtr pRawInputDeviceList = Marshal.AllocHGlobal((int)(dwSize * deviceCount));
              GetRawInputDeviceList(pRawInputDeviceList, ref deviceCount, (uint)dwSize);

              List<DeviceDetails> devices = new List<DeviceDetails>((int)deviceCount);

              // Iterate through the list, discarding undesired items
              // and retrieving further information on keyboard devices
              for (int i = 0; i < deviceCount; i++)
              {
                  string deviceName;
                  uint pcbSize = 0;

                  RAWINPUTDEVICELIST rid;

                  IntPtr location;
                  int offset = dwSize * i;

                  if (IntPtr.Size == 4)
                      location = new IntPtr(pRawInputDeviceList.ToInt32() + offset);
                  else
                      location = new IntPtr(pRawInputDeviceList.ToInt64() + offset);

                  rid = (RAWINPUTDEVICELIST)Marshal.PtrToStructure(location, typeof(RAWINPUTDEVICELIST));

                  GetRawInputDeviceInfo(rid.hDevice, RIDI_DEVICENAME, IntPtr.Zero, ref pcbSize);

                  if (pcbSize > 0)
                  {
                      IntPtr pData = Marshal.AllocHGlobal((int)pcbSize);
                      GetRawInputDeviceInfo(rid.hDevice, RIDI_DEVICENAME, pData, ref pcbSize);
                      deviceName = Marshal.PtrToStringAnsi(pData);

                      // Drop the "root" keyboard and mouse devices used for Terminal Services and the Remote Desktop
                      if (deviceName.ToUpperInvariant().Contains("ROOT"))
                          continue;

                      // If the device is identified in the list as a keyboard or 
                      // HID device, create a DeviceInfo object to store information 
                      // about it

                      // Get Detailed Info ...
                      uint size = (uint)Marshal.SizeOf(typeof(DeviceInfo));
                      DeviceInfo di = new DeviceInfo();
                      di.Size = Marshal.SizeOf(typeof(DeviceInfo));
                      GetRawInputDeviceInfo(rid.hDevice, RIDI_DEVICEINFO, ref di, ref size);

                      di = new DeviceInfo();
                      di.Size = Marshal.SizeOf(typeof(DeviceInfo));
                      GetRawInputDeviceInfo(rid.hDevice, RIDI_DEVICEINFO, ref di, ref size);

                      DeviceDetails details = new DeviceDetails();
                      //details.Name = deviceName;
                      details.ID = deviceName;

                      switch (di.Type)
                      {
                          case RawInputType.HID:
                              {
                                  string vidAndPid = String.Format("Vid_{0:x4}&Pid_{1:x4}", di.HIDInfo.VendorID, di.HIDInfo.ProductID);
                                  details.Name = String.Format("HID: {0}", GetFriendlyName(vidAndPid));
                                  //details.ID = GetDeviceDesc(deviceName);

                                  details.UsagePage = di.HIDInfo.UsagePage;
                                  details.Usage = di.HIDInfo.Usage;

                                  devices.Add(details);
                                  break;
                              }

                          case RawInputType.Keyboard:
                              {
                                  details.Name = "HID Keyboard";
                                  //details.ID = String.Format("{0}-{1}", di.KeyboardInfo.Type, di.KeyboardInfo.SubType);

                                  details.UsagePage = 0x01;
                                  details.Usage = 0x06;

                                  devices.Add(details);
                                  break;
                              }

                          case RawInputType.Mouse:
                              {
                                  details.Name = "HID Mouse";

                                  details.UsagePage = 0x01;
                                  details.Usage = 0x02;

                                  devices.Add(details);
                                  break;
                              }
                      }

                      Marshal.FreeHGlobal(pData);
                  }
              }

              Marshal.FreeHGlobal(pRawInputDeviceList);

              return devices;
          }
          else
          {
              throw new InvalidOperationException("An error occurred while retrieving the list of devices");
          }
      }

      private static string GetFriendlyName(string vidAndPid)
      {
          try
          {
              using (RegistryKey USBEnum = Registry.LocalMachine.OpenSubKey("SYSTEM\\CurrentControlSet\\Enum\\USB"))
              {
                  foreach (string usbSubKey in USBEnum.GetSubKeyNames())
                  {
                      if (usbSubKey.IndexOf(vidAndPid, StringComparison.OrdinalIgnoreCase) == -1)
                          continue;

                      using (RegistryKey currentKey = USBEnum.OpenSubKey(usbSubKey))
                      {
                          string[] vidAndPidSubKeys = currentKey.GetSubKeyNames();

                          foreach (string vidAndPidSubKey in vidAndPidSubKeys)
                          {
                              using (RegistryKey subKey = currentKey.OpenSubKey(vidAndPidSubKey))
                                  return subKey.GetValue("LocationInformation", null) as string;
                          }
                      }
                  }
              }
          }
          catch
          {
          }

          return null;
      }
  }
  #endregion
}