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
using System.Collections;
using System.Drawing;
using System.IO;
using System.Net;
using System.Threading;
using System.Windows.Forms;
using System.Xml;
using IrssComms;
using IrssUtils;
using IrssUtils.Forms;

namespace VirtualRemote
{
  internal partial class MainForm : Form
  {
    #region Variables

    //Thread _updateThread;

    #endregion Variables

    #region Constructor

    public MainForm()
    {
      InitializeComponent();
    }

    #endregion Constructor

    private void MainForm_Load(object sender, EventArgs e)
    {
      SetSkinList();
      SetSkin(Program.RemoteSkin);

      //_updateThread = new Thread(new ThreadStart(SetLabel));
      //_updateThread.Start();
    }

    private void MainForm_FormClosing(object sender, FormClosingEventArgs e)
    {
      //_updateThread.Abort();
    }

    private void MainForm_MouseClick(object sender, MouseEventArgs e)
    {
      if (e.Button != MouseButtons.Left)
        return;

      Program.ProcessClick(e.X, e.Y);
    }

    private void MainForm_KeyDown(object sender, KeyEventArgs e)
    {
      if (e.KeyCode == Keys.None)
        return;

      foreach (RemoteButton button in Program.Buttons)
      {
        if (button.Shortcut == e.KeyCode)
        {
          Program.ButtonPress(button.Code);
          break;
        }
      }
    }

    private static void LoadSkinXml(string xmlFile)
    {
      XmlDocument doc = new XmlDocument();
      doc.Load(xmlFile);

      ArrayList buttons = new ArrayList();
      RemoteButton temp;
      string key = String.Empty;

      XmlNodeList commandSequence = doc.DocumentElement.SelectNodes("button");
      foreach (XmlNode item in commandSequence)
      {
        temp = new RemoteButton();
        temp.Name = item.Attributes["name"].Value;
        temp.Code = item.Attributes["code"].Value;

        try
        {
          key = item.Attributes["shortcut"].Value;
          temp.Shortcut = (Keys) Enum.Parse(typeof (Keys), key, true);
        }
        catch (ArgumentException)
        {
          IrssLog.Error("Invalid Key Shortcut \"{0}\" in skin \"{1}\"", key, xmlFile);
        }

        temp.Top = int.Parse(item.Attributes["top"].Value);
        temp.Left = int.Parse(item.Attributes["left"].Value);
        temp.Height = int.Parse(item.Attributes["height"].Value);
        temp.Width = int.Parse(item.Attributes["width"].Value);
        buttons.Add(temp);
      }

      Program.Buttons = (RemoteButton[]) buttons.ToArray(typeof (RemoteButton));
    }

    private void SetSkinList()
    {
      try
      {
        string[] skins = Directory.GetFiles(Program.SkinsFolder, "*.png", SearchOption.TopDirectoryOnly);
        for (int index = 0; index < skins.Length; index++)
          skins[index] = Path.GetFileNameWithoutExtension(skins[index]);

        toolStripComboBoxSkin.Items.Clear();
        toolStripComboBoxSkin.Items.AddRange(skins);

        toolStripComboBoxSkin.SelectedItem = Program.RemoteSkin;
      }
      catch (Exception ex)
      {
        IrssLog.Error(ex);
      }
    }

    private void SetSkin(string skin)
    {
      try
      {
        if (String.IsNullOrEmpty(skin))
          return;

        string skinFile = Path.Combine(Program.SkinsFolder, skin + ".png");
        if (!File.Exists(skinFile))
          throw new FileNotFoundException("Skin graphic file not found", skinFile);

        // Try to load xml file of same name, failing that load using first word of skin name ...
        string xmlFile = Path.Combine(Program.SkinsFolder, skin + ".xml");
        if (!File.Exists(xmlFile))
        {
          string firstWord = skin.Split(new char[] {' '}, StringSplitOptions.RemoveEmptyEntries)[0];

          xmlFile = Path.Combine(Program.SkinsFolder, firstWord + ".xml");
          if (!File.Exists(xmlFile))
            throw new FileNotFoundException("Skin file not found", xmlFile);
        }

        Program.Device = Path.GetFileNameWithoutExtension(xmlFile);
        LoadSkinXml(xmlFile);

        BackgroundImage = new Bitmap(skinFile);
        ClientSize = new Size(BackgroundImage.Width, BackgroundImage.Height);

        Program.RemoteSkin = skin;
      }
      catch (Exception ex)
      {
        IrssLog.Error(ex);
        MessageBox.Show(this, ex.Message, "Virtual Remote - Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
      }
    }

    private void SetLabel()
    {
      while (true)
      {
        labelDisabled.Visible = !Program.Registered;
        Thread.Sleep(2500);
      }
    }

    private void toolStripMenuItemQuit_Click(object sender, EventArgs e)
    {
      IrssLog.Info("User quit");

      Close();
    }

    private void toolStripComboBoxSkin_SelectedIndexChanged(object sender, EventArgs e)
    {
      SetSkin(toolStripComboBoxSkin.SelectedItem as string);
      contextMenuStrip.Hide();
    }

    private void changeServerHostToolStripMenuItem_Click(object sender, EventArgs e)
    {
      Program.StopClient();

      ServerAddress serverAddress = new ServerAddress(Program.ServerHost);
      serverAddress.ShowDialog(this);

      Program.ServerHost = serverAddress.ServerHost;

      IPAddress serverIP = Network.GetIPFromName(Program.ServerHost);
      IPEndPoint endPoint = new IPEndPoint(serverIP, Server.DefaultPort);

      Program.StartClient(endPoint);
    }

    private void toolStripMenuItemHelp_Click(object sender, EventArgs e)
    {
      try
      {
        string file = Path.Combine(SystemRegistry.GetInstallFolder(), "IR Server Suite.chm");
        Help.ShowHelp(this, file, HelpNavigator.Topic, "Virtual Remote\\index.html");
      }
      catch (Exception ex)
      {
        IrssLog.Error(ex);
        MessageBox.Show(this, ex.Message, "Failed to load help", MessageBoxButtons.OK, MessageBoxIcon.Error);
      }
    }
  }
}