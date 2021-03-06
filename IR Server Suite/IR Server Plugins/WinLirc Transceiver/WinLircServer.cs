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
using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using System.Text;

namespace IRServer.Plugin
{
  /// <summary>
  /// WinLIRC server class implementing communication with WinLIRC.
  /// All remotes are supported as long as WinLIRC supports them.
  /// </summary>
  internal class WinLircServer
  {
    #region Variables

    #region Delegates

    public delegate void CommandEventHandler(WinLircCommand cmd);

    #endregion

    private readonly TimeSpan _buttonReleaseTime;
    // Time span in which multiple receptions of the same command are ignored

    private readonly int _repeatDelay; // Number of repeats to ignore before repeating
    private readonly Socket _socket; // Socket for WinLIRC communication
    private AsyncCallback _dataCallback; // Callback function receiving data from WinLIRC
    private IAsyncResult _dataCallbackResult; // Result of the callback function
    private WinLircCommand _lastCommand; // Last command actually sent to InputHandler
    public event CommandEventHandler CommandEvent;

    #endregion Variables

    #region Constructors + Initialization

    /// <summary>
    /// Initializes a new instance of the <see cref="WinLircServer"/> class.
    /// </summary>
    /// <param name="ip">The ip address.</param>
    /// <param name="port">The port.</param>
    /// <param name="buttonReleaseTime">The button release time.</param>
    /// <param name="repeatDelay">The repeat delay.</param>
    public WinLircServer(IPAddress ip, int port, TimeSpan buttonReleaseTime, int repeatDelay)
    {
      _buttonReleaseTime = buttonReleaseTime;
      _repeatDelay = repeatDelay;
      _lastCommand = new WinLircCommand();

      _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
      _socket.Connect(ip, port); // Connect; error handling is done in SetupDataCallback()

      SetupDataCallback(); // Setup callback function that will receive data
    }

    /// <summary>
    /// Set up callback function receiving data from WinLIRC
    /// </summary>
    private void SetupDataCallback()
    {
      try
      {
        if (_dataCallback == null)
          _dataCallback = OnDataReceived;

        SocketInfo info = new SocketInfo(_socket);
        _dataCallbackResult = _socket.BeginReceive(info.DataBuffer, 0, info.DataBuffer.Length, SocketFlags.None,
                                                   _dataCallback, info);
      }
      catch (SocketException se)
      {
        Trace.WriteLine("WLirc: Error listening to socket: " + se.Message);
      }
    }

    #endregion Constructors + Initialization

    #region Public Methods

    /// <summary>
    /// Starts the server application.
    /// </summary>
    /// <param name="path">The path to the server application.</param>
    /// <returns><c>true</c> if successful; otherwise <c>false</c>.</returns>
    public static bool StartServer(string path)
    {
      if (IsServerRunning())
      {
        Trace.WriteLine("WLirc: WinLIRC server was not started (already running)");
      }
      else
      {
        Trace.WriteLine("WLirc: Starting WinLIRC server...");
        try
        {
          Process.Start(path);
        }
        catch (Exception)
        {
          Trace.WriteLine("WLirc: WinLIRC server start failed");
          return false;
        }
      }

      return true;
    }

    /// <summary>
    /// Determines whether the server application is running.
    /// </summary>
    /// <returns><c>true</c> if the server is running; otherwise, <c>false</c>.</returns>
    public static bool IsServerRunning()
    {
      Process[] processes = Process.GetProcessesByName("winlirc");
      return (processes.Length > 0);
    }

    /// <summary>
    /// Transmits the specified data to the winlirc server.
    /// </summary>
    /// <param name="transmit">The data to transmit.</param>
    public void Transmit(string transmit)
    {
      _socket.Send(Encoding.ASCII.GetBytes(transmit));
    }

    #endregion Public Methods

    #region Private Methods

    /// <summary>
    /// Callback function receiving data from WinLIRC
    /// </summary>
    private void OnDataReceived(IAsyncResult async)
    {
      try
      {
        SocketInfo info = (SocketInfo) async.AsyncState;
        int receivedBytesCount = info.Sock.EndReceive(async);

        // Convert received bytes to string
        char[] chars = new char[receivedBytesCount + 1];
        Decoder decoder = Encoding.UTF8.GetDecoder();
        decoder.GetChars(info.DataBuffer, 0, receivedBytesCount, chars, 0);
        string data = new string(chars);

        string[] commands = data.Split(new char[] {'\n', '\r', '\0'}, StringSplitOptions.RemoveEmptyEntries);
        foreach (string cmd in commands)
          ProcessData(cmd);

        SetupDataCallback(); // Listen to new signals again
      }
      catch (ObjectDisposedException)
      {
        Trace.WriteLine("WLirc: OnDataReceived: Socket has been closed");
      }
      catch (SocketException se)
      {
        Trace.WriteLine("WLirc: OnDataReceived: Socket exception: " + se.Message);
      }
    }

    /// <summary>
    /// Process received data, i.e. send event to event handlers
    /// </summary>
    private void ProcessData(string data)
    {
      // Ignore commands we do not need (like the startup message)
      if (data.Equals("BEGIN", StringComparison.OrdinalIgnoreCase) ||
          data.Equals("END", StringComparison.OrdinalIgnoreCase) ||
          data.Equals("SIGHUP", StringComparison.OrdinalIgnoreCase))
        return;

      WinLircCommand command = new WinLircCommand(data);

      #region Time-based repeat filter

      if (_lastCommand.IsSameCommand(command))
      {
        if (_repeatDelay > 0 && command.Repeats > 0
            && command.Repeats < _repeatDelay)
        {
          Trace.WriteLine(String.Format("WLirc: Command '{0}' ignored because of repeat delay filter", command.Button));
          return;
        }
        if ((command.Time - _lastCommand.Time) < _buttonReleaseTime)
        {
          Trace.WriteLine(String.Format("WLirc: Command '{0}' ignored because of repeat filter", command.Button));
          return;
        }
      }

      #endregion Time-based repeat filter

      Trace.WriteLine(String.Format("WLirc: Command '{0}' accepted", command.Button));
      _lastCommand = command;

      if (CommandEvent != null)
        CommandEvent(command);
    }

    #endregion Private Methods
  }
}