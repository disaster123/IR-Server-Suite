#region Using directives

using System;
using System.Drawing;
using System.Collections;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.Xml;

#endregion Using directives

namespace VirtualRemote
{

  /// <summary>
  /// Main Form.
  /// </summary>
  class FormMain : Form
  {

    #region Constants

    static readonly string ConfigurationFile = "VirtualRemote.xml";

    const int ServerPort = 24000;

    static readonly string[][] Buttons = new string[][] {
      new string[] { "Back", "Up", "Info", "Left", "OK", "Right", "VolumeUp", "Down", "ChannelUp", "VolumeDown", "Mute", "ChannelDown" },
      new string[] { "Record", "Stop", "Pause", "Rewind", "Play", "FastForward", "PreviousChapter", null, "NextChapter", null, null, null },
      new string[] { "Number1", "Number2", "Number3", "Number4", "Number5", "Number6", "Number7", "Number8", "Number9", "Star", "Number0", "Hash", },
      new string[] { "Power", "TV", "Red", "Music", "Videos", "Green", "Pictures", "Guide", "Yellow", "DVD", "Teletext", "Blue" }
    };

    #endregion Constants

    #region Variables

    Client _client;

    string _serverHost = "192.168.0.3";

    bool _registered;

    int _page = 0;

    private MainMenu mainMenu;
    private MenuItem menuItemQuit;
    private MenuItem menuItemOptions;
    private MenuItem menuItemSetup;
    private MenuItem menuItemConnect;
    private MenuItem menuItemDisconnect;
    private PictureBox pictureBoxNumbers;
    private PictureBox pictureBoxNavigation;
    private PictureBox pictureBoxPlayback;
    private Label labelStatus;
    private PictureBox pictureBoxMisc;

    #endregion Variables

    #region Constructor

    /// <summary>
    /// Initializes a new instance of the <see cref="FormMain"/> class.
    /// </summary>
    public FormMain()
    {
      InitializeComponent();

      LoadSettings();

      SetPageImage();
    }

    #endregion Constructor

    void ReceivedMessage(IrssMessage received)
    {
      switch (received.Type)
      {
        case MessageType.RegisterClient:
          if ((received.Flags & MessageFlags.Success) == MessageFlags.Success)
          {
            _registered = true;
            labelStatus.Text = "Connected";
          }
          else if ((received.Flags & MessageFlags.Failure) == MessageFlags.Failure)
          {
            _registered = false;
            labelStatus.Text = "Failed to negotiate connection";
          }
          return;

        case MessageType.ServerShutdown:
          _registered = false;
          labelStatus.Text = "Server Shutdown";
          return;

        case MessageType.Error:
          labelStatus.Text = received.GetDataAsString();
          return;
      }
    }

    void CommsFailure(object obj)
    {
      StopClient();

      Exception ex = obj as Exception;

      if (ex != null)
        labelStatus.Text = ex.Message;
      else
        labelStatus.Text = "Communications failure";
    }
    void Connected(object obj)
    {
      IrssMessage message = new IrssMessage(MessageType.RegisterClient, MessageFlags.Request);
      _client.Send(message);
    }
    void Disconnected(object obj)
    {
      labelStatus.Text = "Communications with server has been lost";

      Thread.Sleep(1000);
    }

    bool StartClient(IPEndPoint endPoint)
    {
      if (_client != null)
        return false;

      ClientMessageSink sink = new ClientMessageSink(ReceivedMessage);

      _client = new Client(endPoint, sink);
      _client.CommsFailureCallback  = new WaitCallback(CommsFailure);
      _client.ConnectCallback       = new WaitCallback(Connected);
      _client.DisconnectCallback    = new WaitCallback(Disconnected);
      
      if (_client.Start())
      {
        return true;
      }
      else
      {
        _client = null;
        return false;
      }
    }
    void StopClient()
    {
      if (_client == null)
        return;

      _client.Dispose();
      _client = null;

      _registered = false;
    }

    void ButtonPress(string keyCode)
    {
      if (!_registered)
        return;

      if (keyCode == null)
        return;

      byte[] deviceNameBytes = Encoding.ASCII.GetBytes("Abstract");
      byte[] keyCodeBytes = Encoding.ASCII.GetBytes(keyCode);

      byte[] bytes = new byte[8 + deviceNameBytes.Length + keyCodeBytes.Length];

      BitConverter.GetBytes(deviceNameBytes.Length).CopyTo(bytes, 0);
      deviceNameBytes.CopyTo(bytes, 4);
      BitConverter.GetBytes(keyCodeBytes.Length).CopyTo(bytes, 4 + deviceNameBytes.Length);
      keyCodeBytes.CopyTo(bytes, 8 + deviceNameBytes.Length);

      IrssMessage message = new IrssMessage(MessageType.ForwardRemoteEvent, MessageFlags.Notify, bytes);
      _client.Send(message);
    }

    void Quit()
    {
      StopClient();
      Application.Exit();
    }


    void LoadSettings()
    {
      XmlDocument doc = new XmlDocument();

      try
      {
        doc.Load(ConfigurationFile);
      }
      catch
      {
        _serverHost = "192.168.0.1";
        return;
      }

      try { _serverHost = doc.DocumentElement.Attributes["ServerHost"].Value; }
      catch { _serverHost = "192.168.0.1"; }
    }
    void SaveSettings()
    {
      XmlTextWriter writer = new XmlTextWriter(ConfigurationFile, Encoding.UTF8);
      writer.Formatting = Formatting.Indented;
      writer.Indentation = 1;
      writer.IndentChar = (char)9;
      writer.WriteStartDocument(true);
      writer.WriteStartElement("settings"); // <settings>

      writer.WriteAttributeString("ServerHost", _serverHost);

      writer.WriteEndElement(); // </settings>
      writer.WriteEndDocument();

      writer.Close();
    }

    void PagePrevious()
    {
      _page--;

      if (_page < 0)
        _page += Buttons.GetLength(0);

      SetPageImage();
    }
    void PageNext()
    {
      _page++;

      if (_page >= Buttons.GetLength(0))
        _page -= Buttons.GetLength(0);

      SetPageImage();
    }

    void SetPageImage()
    {
      switch (_page)
      {
        case 0:
          pictureBoxNavigation.Visible  = true;
          pictureBoxPlayback.Visible    = false;
          pictureBoxNumbers.Visible     = false;
          pictureBoxMisc.Visible        = false;
          break;

        case 1:
          pictureBoxNavigation.Visible  = false;
          pictureBoxPlayback.Visible    = true;
          pictureBoxNumbers.Visible     = false;
          pictureBoxMisc.Visible        = false;
          break;

        case 2:
          pictureBoxNavigation.Visible  = false;
          pictureBoxPlayback.Visible    = false;
          pictureBoxNumbers.Visible     = true;
          pictureBoxMisc.Visible        = false;
          break;

        case 3:
          pictureBoxNavigation.Visible  = false;
          pictureBoxPlayback.Visible    = false;
          pictureBoxNumbers.Visible     = false;
          pictureBoxMisc.Visible        = true;
          break;
      }
    }



    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    protected override void Dispose(bool disposing)
    {
      base.Dispose(disposing);
    }

    #region Windows Form Designer generated code
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent()
    {
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormMain));
      this.mainMenu = new System.Windows.Forms.MainMenu();
      this.menuItemQuit = new System.Windows.Forms.MenuItem();
      this.menuItemOptions = new System.Windows.Forms.MenuItem();
      this.menuItemSetup = new System.Windows.Forms.MenuItem();
      this.menuItemConnect = new System.Windows.Forms.MenuItem();
      this.menuItemDisconnect = new System.Windows.Forms.MenuItem();
      this.pictureBoxNumbers = new System.Windows.Forms.PictureBox();
      this.pictureBoxNavigation = new System.Windows.Forms.PictureBox();
      this.pictureBoxPlayback = new System.Windows.Forms.PictureBox();
      this.pictureBoxMisc = new System.Windows.Forms.PictureBox();
      this.labelStatus = new System.Windows.Forms.Label();
      // 
      // mainMenu
      // 
      this.mainMenu.MenuItems.Add(this.menuItemQuit);
      this.mainMenu.MenuItems.Add(this.menuItemOptions);
      // 
      // menuItemQuit
      // 
      this.menuItemQuit.Text = "Quit";
      this.menuItemQuit.Click += new System.EventHandler(this.menuItemQuit_Click);
      // 
      // menuItemOptions
      // 
      this.menuItemOptions.MenuItems.Add(this.menuItemSetup);
      this.menuItemOptions.MenuItems.Add(this.menuItemConnect);
      this.menuItemOptions.MenuItems.Add(this.menuItemDisconnect);
      this.menuItemOptions.Text = "Options";
      // 
      // menuItemSetup
      // 
      this.menuItemSetup.Text = "Setup";
      this.menuItemSetup.Click += new System.EventHandler(this.menuItemSetup_Click);
      // 
      // menuItemConnect
      // 
      this.menuItemConnect.Text = "Connect";
      this.menuItemConnect.Click += new System.EventHandler(this.menuItemConnect_Click);
      // 
      // menuItemDisconnect
      // 
      this.menuItemDisconnect.Text = "Disconnect";
      this.menuItemDisconnect.Click += new System.EventHandler(this.menuItemDisconnect_Click);
      // 
      // pictureBoxNumbers
      // 
      this.pictureBoxNumbers.Image = ((System.Drawing.Image)(resources.GetObject("pictureBoxNumbers.Image")));
      this.pictureBoxNumbers.Location = new System.Drawing.Point(0, 48);
      this.pictureBoxNumbers.Size = new System.Drawing.Size(176, 131);
      this.pictureBoxNumbers.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
      // 
      // pictureBoxNavigation
      // 
      this.pictureBoxNavigation.Image = ((System.Drawing.Image)(resources.GetObject("pictureBoxNavigation.Image")));
      this.pictureBoxNavigation.Location = new System.Drawing.Point(0, 48);
      this.pictureBoxNavigation.Size = new System.Drawing.Size(176, 131);
      this.pictureBoxNavigation.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
      // 
      // pictureBoxPlayback
      // 
      this.pictureBoxPlayback.Image = ((System.Drawing.Image)(resources.GetObject("pictureBoxPlayback.Image")));
      this.pictureBoxPlayback.Location = new System.Drawing.Point(0, 48);
      this.pictureBoxPlayback.Size = new System.Drawing.Size(176, 131);
      this.pictureBoxPlayback.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
      // 
      // pictureBoxMisc
      // 
      this.pictureBoxMisc.Image = ((System.Drawing.Image)(resources.GetObject("pictureBoxMisc.Image")));
      this.pictureBoxMisc.Location = new System.Drawing.Point(0, 48);
      this.pictureBoxMisc.Size = new System.Drawing.Size(176, 131);
      this.pictureBoxMisc.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
      // 
      // labelStatus
      // 
      this.labelStatus.Location = new System.Drawing.Point(0, 0);
      this.labelStatus.Size = new System.Drawing.Size(176, 48);
      this.labelStatus.Text = "Not connected";
      this.labelStatus.TextAlign = System.Drawing.ContentAlignment.TopCenter;
      // 
      // FormMain
      // 
      this.ClientSize = new System.Drawing.Size(176, 180);
      this.Controls.Add(this.pictureBoxMisc);
      this.Controls.Add(this.pictureBoxNumbers);
      this.Controls.Add(this.pictureBoxPlayback);
      this.Controls.Add(this.pictureBoxNavigation);
      this.Controls.Add(this.labelStatus);
      this.Menu = this.mainMenu;
      this.Text = "Virtual Remote";
      this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.FormMain_KeyDown);

    }

    #endregion

    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    static void Main()
    {
      Application.Run(new FormMain());
    }


    private void menuItemQuit_Click(object sender, EventArgs e)
    {
      Quit();
    }

    private void menuItemSetup_Click(object sender, EventArgs e)
    {
      ServerAddress serverAddress = new ServerAddress(_serverHost);
      if (serverAddress.ShowDialog() == DialogResult.OK)
      {
        _serverHost = serverAddress.ServerHost;
        SaveSettings();
      }
    }

    private void menuItemConnect_Click(object sender, EventArgs e)
    {
      labelStatus.Text = String.Format("Connecting to {0} ...", _serverHost);

      IPAddress serverIP = IPAddress.Parse(_serverHost);
      IPEndPoint endPoint = new IPEndPoint(serverIP, ServerPort);

      StartClient(endPoint);
    }

    private void menuItemDisconnect_Click(object sender, EventArgs e)
    {
      StopClient();

      labelStatus.Text = "Disconnected";
    }

    private void FormMain_KeyDown(object sender, KeyEventArgs e)
    {
      if (e.KeyCode == Keys.Left)
      {
        PagePrevious();
      }
      else if (e.KeyCode == Keys.Right)
      {
        PageNext();
      }
      else
      {
        switch (e.KeyCode)
        {
          case Keys.D1: ButtonPress(Buttons[_page][0]); break;
          case Keys.D2: ButtonPress(Buttons[_page][1]); break;
          case Keys.D3: ButtonPress(Buttons[_page][2]); break;
          case Keys.D4: ButtonPress(Buttons[_page][3]); break;
          case Keys.D5: ButtonPress(Buttons[_page][4]); break;
          case Keys.D6: ButtonPress(Buttons[_page][5]); break;
          case Keys.D7: ButtonPress(Buttons[_page][6]); break;
          case Keys.D8: ButtonPress(Buttons[_page][7]); break;
          case Keys.D9: ButtonPress(Buttons[_page][8]); break;
          case Keys.F8: ButtonPress(Buttons[_page][9]); break;
          case Keys.D0: ButtonPress(Buttons[_page][10]); break;
          case Keys.F9: ButtonPress(Buttons[_page][11]); break;
        }
      }
    }


  }

}
