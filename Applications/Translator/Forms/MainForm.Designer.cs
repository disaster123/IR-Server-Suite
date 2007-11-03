namespace Translator
{

  partial class MainForm
  {

    /// <summary>
    /// Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override void Dispose(bool disposing)
    {
      if (disposing && (components != null))
      {
        components.Dispose();
      }
      base.Dispose(disposing);
    }

    #region Windows Form Designer generated code

    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent()
    {
      this.components = new System.ComponentModel.Container();
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
      this.listViewPrograms = new System.Windows.Forms.ListView();
      this.contextMenuStripPrograms = new System.Windows.Forms.ContextMenuStrip(this.components);
      this.imageListPrograms = new System.Windows.Forms.ImageList(this.components);
      this.buttonClear = new System.Windows.Forms.Button();
      this.buttonModify = new System.Windows.Forms.Button();
      this.buttonDelete = new System.Windows.Forms.Button();
      this.buttonNew = new System.Windows.Forms.Button();
      this.listViewButtons = new System.Windows.Forms.ListView();
      this.columnHeaderButton = new System.Windows.Forms.ColumnHeader();
      this.columnHeaderDescription = new System.Windows.Forms.ColumnHeader();
      this.columnHeaderCommand = new System.Windows.Forms.ColumnHeader();
      this.buttonOK = new System.Windows.Forms.Button();
      this.tabControl = new System.Windows.Forms.TabControl();
      this.tabPagePrograms = new System.Windows.Forms.TabPage();
      this.tabPageEvents = new System.Windows.Forms.TabPage();
      this.buttonSetCommand = new System.Windows.Forms.Button();
      this.buttonAddEvent = new System.Windows.Forms.Button();
      this.labelCommand = new System.Windows.Forms.Label();
      this.comboBoxCommands = new System.Windows.Forms.ComboBox();
      this.labelEvent = new System.Windows.Forms.Label();
      this.comboBoxEvents = new System.Windows.Forms.ComboBox();
      this.listViewEventMap = new System.Windows.Forms.ListView();
      this.columnHeaderEvent = new System.Windows.Forms.ColumnHeader();
      this.columnHeaderEventCommand = new System.Windows.Forms.ColumnHeader();
      this.contextMenuStripEvents = new System.Windows.Forms.ContextMenuStrip(this.components);
      this.removeEventToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.tabPageMacro = new System.Windows.Forms.TabPage();
      this.listViewMacro = new System.Windows.Forms.ListView();
      this.buttonTestMacro = new System.Windows.Forms.Button();
      this.buttonDeleteMacro = new System.Windows.Forms.Button();
      this.buttonNewMacro = new System.Windows.Forms.Button();
      this.buttonEditMacro = new System.Windows.Forms.Button();
      this.tabPageIRCommands = new System.Windows.Forms.TabPage();
      this.listViewIR = new System.Windows.Forms.ListView();
      this.buttonNewIR = new System.Windows.Forms.Button();
      this.buttonEditIR = new System.Windows.Forms.Button();
      this.buttonDeleteIR = new System.Windows.Forms.Button();
      this.toolTip = new System.Windows.Forms.ToolTip(this.components);
      this.checkBoxAutoRun = new System.Windows.Forms.CheckBox();
      this.menuStrip = new System.Windows.Forms.MenuStrip();
      this.configurationToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.newToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.importToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.exportToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
      this.serverToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
      this.quitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.helpToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.translatorHelpToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.aboutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
      this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
      this.tabControl.SuspendLayout();
      this.tabPagePrograms.SuspendLayout();
      this.tabPageEvents.SuspendLayout();
      this.contextMenuStripEvents.SuspendLayout();
      this.tabPageMacro.SuspendLayout();
      this.tabPageIRCommands.SuspendLayout();
      this.menuStrip.SuspendLayout();
      this.SuspendLayout();
      // 
      // listViewPrograms
      // 
      this.listViewPrograms.Alignment = System.Windows.Forms.ListViewAlignment.Left;
      this.listViewPrograms.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.listViewPrograms.ContextMenuStrip = this.contextMenuStripPrograms;
      this.listViewPrograms.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None;
      this.listViewPrograms.HideSelection = false;
      this.listViewPrograms.LargeImageList = this.imageListPrograms;
      this.listViewPrograms.Location = new System.Drawing.Point(8, 8);
      this.listViewPrograms.MultiSelect = false;
      this.listViewPrograms.Name = "listViewPrograms";
      this.listViewPrograms.ShowGroups = false;
      this.listViewPrograms.ShowItemToolTips = true;
      this.listViewPrograms.Size = new System.Drawing.Size(504, 80);
      this.listViewPrograms.TabIndex = 0;
      this.listViewPrograms.TileSize = new System.Drawing.Size(128, 48);
      this.toolTip.SetToolTip(this.listViewPrograms, "Choose a Program to modify mappings for here");
      this.listViewPrograms.UseCompatibleStateImageBehavior = false;
      this.listViewPrograms.DoubleClick += new System.EventHandler(this.listViewPrograms_DoubleClick);
      this.listViewPrograms.SelectedIndexChanged += new System.EventHandler(this.listViewPrograms_SelectedIndexChanged);
      // 
      // contextMenuStripPrograms
      // 
      this.contextMenuStripPrograms.Name = "contextMenuStripPrograms";
      this.contextMenuStripPrograms.Size = new System.Drawing.Size(61, 4);
      this.contextMenuStripPrograms.Opening += new System.ComponentModel.CancelEventHandler(this.contextMenuStripPrograms_Opening);
      // 
      // imageListPrograms
      // 
      this.imageListPrograms.ColorDepth = System.Windows.Forms.ColorDepth.Depth32Bit;
      this.imageListPrograms.ImageSize = new System.Drawing.Size(32, 32);
      this.imageListPrograms.TransparentColor = System.Drawing.Color.Transparent;
      // 
      // buttonClear
      // 
      this.buttonClear.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonClear.Location = new System.Drawing.Point(456, 344);
      this.buttonClear.Name = "buttonClear";
      this.buttonClear.Size = new System.Drawing.Size(56, 24);
      this.buttonClear.TabIndex = 5;
      this.buttonClear.Text = "Clear";
      this.toolTip.SetToolTip(this.buttonClear, "Clear all button mappings");
      this.buttonClear.UseVisualStyleBackColor = true;
      this.buttonClear.Click += new System.EventHandler(this.buttonClear_Click);
      // 
      // buttonModify
      // 
      this.buttonModify.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonModify.Location = new System.Drawing.Point(72, 344);
      this.buttonModify.Name = "buttonModify";
      this.buttonModify.Size = new System.Drawing.Size(56, 24);
      this.buttonModify.TabIndex = 3;
      this.buttonModify.Text = "Edit";
      this.toolTip.SetToolTip(this.buttonModify, "Edit the currently selected button mapping");
      this.buttonModify.UseVisualStyleBackColor = true;
      this.buttonModify.Click += new System.EventHandler(this.buttonModify_Click);
      // 
      // buttonDelete
      // 
      this.buttonDelete.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonDelete.Location = new System.Drawing.Point(136, 344);
      this.buttonDelete.Name = "buttonDelete";
      this.buttonDelete.Size = new System.Drawing.Size(56, 24);
      this.buttonDelete.TabIndex = 4;
      this.buttonDelete.Text = "Delete";
      this.toolTip.SetToolTip(this.buttonDelete, "Delete the currently selected button mapping");
      this.buttonDelete.UseVisualStyleBackColor = true;
      this.buttonDelete.Click += new System.EventHandler(this.buttonDelete_Click);
      // 
      // buttonNew
      // 
      this.buttonNew.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonNew.Location = new System.Drawing.Point(8, 344);
      this.buttonNew.Name = "buttonNew";
      this.buttonNew.Size = new System.Drawing.Size(56, 24);
      this.buttonNew.TabIndex = 2;
      this.buttonNew.Text = "New";
      this.toolTip.SetToolTip(this.buttonNew, "Create a new button mapping");
      this.buttonNew.UseVisualStyleBackColor = true;
      this.buttonNew.Click += new System.EventHandler(this.buttonNew_Click);
      // 
      // listViewButtons
      // 
      this.listViewButtons.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.listViewButtons.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeaderButton,
            this.columnHeaderDescription,
            this.columnHeaderCommand});
      this.listViewButtons.FullRowSelect = true;
      this.listViewButtons.GridLines = true;
      this.listViewButtons.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
      this.listViewButtons.Location = new System.Drawing.Point(8, 96);
      this.listViewButtons.MultiSelect = false;
      this.listViewButtons.Name = "listViewButtons";
      this.listViewButtons.Size = new System.Drawing.Size(504, 240);
      this.listViewButtons.TabIndex = 1;
      this.listViewButtons.UseCompatibleStateImageBehavior = false;
      this.listViewButtons.View = System.Windows.Forms.View.Details;
      this.listViewButtons.DoubleClick += new System.EventHandler(this.listViewButtons_DoubleClick);
      this.listViewButtons.KeyDown += new System.Windows.Forms.KeyEventHandler(this.listViewButtons_KeyDown);
      // 
      // columnHeaderButton
      // 
      this.columnHeaderButton.Text = "Code";
      this.columnHeaderButton.Width = 100;
      // 
      // columnHeaderDescription
      // 
      this.columnHeaderDescription.Text = "Description";
      this.columnHeaderDescription.Width = 180;
      // 
      // columnHeaderCommand
      // 
      this.columnHeaderCommand.Text = "Command";
      this.columnHeaderCommand.Width = 200;
      // 
      // buttonOK
      // 
      this.buttonOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonOK.DialogResult = System.Windows.Forms.DialogResult.OK;
      this.buttonOK.Location = new System.Drawing.Point(472, 440);
      this.buttonOK.Name = "buttonOK";
      this.buttonOK.Size = new System.Drawing.Size(64, 24);
      this.buttonOK.TabIndex = 3;
      this.buttonOK.Text = "&OK";
      this.buttonOK.UseVisualStyleBackColor = true;
      this.buttonOK.Click += new System.EventHandler(this.buttonOK_Click);
      // 
      // tabControl
      // 
      this.tabControl.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.tabControl.Controls.Add(this.tabPagePrograms);
      this.tabControl.Controls.Add(this.tabPageEvents);
      this.tabControl.Controls.Add(this.tabPageMacro);
      this.tabControl.Controls.Add(this.tabPageIRCommands);
      this.tabControl.Location = new System.Drawing.Point(8, 32);
      this.tabControl.Name = "tabControl";
      this.tabControl.SelectedIndex = 0;
      this.tabControl.Size = new System.Drawing.Size(528, 400);
      this.tabControl.TabIndex = 1;
      this.tabControl.SelectedIndexChanged += new System.EventHandler(this.tabControl_SelectedIndexChanged);
      // 
      // tabPagePrograms
      // 
      this.tabPagePrograms.Controls.Add(this.buttonClear);
      this.tabPagePrograms.Controls.Add(this.buttonModify);
      this.tabPagePrograms.Controls.Add(this.buttonDelete);
      this.tabPagePrograms.Controls.Add(this.buttonNew);
      this.tabPagePrograms.Controls.Add(this.listViewButtons);
      this.tabPagePrograms.Controls.Add(this.listViewPrograms);
      this.tabPagePrograms.Location = new System.Drawing.Point(4, 22);
      this.tabPagePrograms.Name = "tabPagePrograms";
      this.tabPagePrograms.Padding = new System.Windows.Forms.Padding(3);
      this.tabPagePrograms.Size = new System.Drawing.Size(520, 374);
      this.tabPagePrograms.TabIndex = 0;
      this.tabPagePrograms.Text = "Programs";
      this.tabPagePrograms.UseVisualStyleBackColor = true;
      // 
      // tabPageEvents
      // 
      this.tabPageEvents.Controls.Add(this.buttonSetCommand);
      this.tabPageEvents.Controls.Add(this.buttonAddEvent);
      this.tabPageEvents.Controls.Add(this.labelCommand);
      this.tabPageEvents.Controls.Add(this.comboBoxCommands);
      this.tabPageEvents.Controls.Add(this.labelEvent);
      this.tabPageEvents.Controls.Add(this.comboBoxEvents);
      this.tabPageEvents.Controls.Add(this.listViewEventMap);
      this.tabPageEvents.Location = new System.Drawing.Point(4, 22);
      this.tabPageEvents.Name = "tabPageEvents";
      this.tabPageEvents.Padding = new System.Windows.Forms.Padding(3);
      this.tabPageEvents.Size = new System.Drawing.Size(520, 374);
      this.tabPageEvents.TabIndex = 1;
      this.tabPageEvents.Text = "Events";
      this.tabPageEvents.UseVisualStyleBackColor = true;
      // 
      // buttonSetCommand
      // 
      this.buttonSetCommand.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonSetCommand.Location = new System.Drawing.Point(456, 344);
      this.buttonSetCommand.Name = "buttonSetCommand";
      this.buttonSetCommand.Size = new System.Drawing.Size(56, 24);
      this.buttonSetCommand.TabIndex = 6;
      this.buttonSetCommand.Text = "Set";
      this.buttonSetCommand.UseVisualStyleBackColor = true;
      this.buttonSetCommand.Click += new System.EventHandler(this.buttonSetCommand_Click);
      // 
      // buttonAddEvent
      // 
      this.buttonAddEvent.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.buttonAddEvent.Location = new System.Drawing.Point(456, 312);
      this.buttonAddEvent.Name = "buttonAddEvent";
      this.buttonAddEvent.Size = new System.Drawing.Size(56, 24);
      this.buttonAddEvent.TabIndex = 3;
      this.buttonAddEvent.Text = "Add";
      this.buttonAddEvent.UseVisualStyleBackColor = true;
      this.buttonAddEvent.Click += new System.EventHandler(this.buttonAddEvent_Click);
      // 
      // labelCommand
      // 
      this.labelCommand.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.labelCommand.Location = new System.Drawing.Point(8, 344);
      this.labelCommand.Name = "labelCommand";
      this.labelCommand.Size = new System.Drawing.Size(80, 20);
      this.labelCommand.TabIndex = 4;
      this.labelCommand.Text = "Command:";
      this.labelCommand.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
      // 
      // comboBoxCommands
      // 
      this.comboBoxCommands.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.comboBoxCommands.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
      this.comboBoxCommands.FormattingEnabled = true;
      this.comboBoxCommands.Location = new System.Drawing.Point(88, 344);
      this.comboBoxCommands.Name = "comboBoxCommands";
      this.comboBoxCommands.Size = new System.Drawing.Size(352, 21);
      this.comboBoxCommands.TabIndex = 5;
      // 
      // labelEvent
      // 
      this.labelEvent.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.labelEvent.Location = new System.Drawing.Point(8, 312);
      this.labelEvent.Name = "labelEvent";
      this.labelEvent.Size = new System.Drawing.Size(80, 21);
      this.labelEvent.TabIndex = 1;
      this.labelEvent.Text = "New event:";
      this.labelEvent.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
      // 
      // comboBoxEvents
      // 
      this.comboBoxEvents.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.comboBoxEvents.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
      this.comboBoxEvents.FormattingEnabled = true;
      this.comboBoxEvents.Location = new System.Drawing.Point(88, 312);
      this.comboBoxEvents.Name = "comboBoxEvents";
      this.comboBoxEvents.Size = new System.Drawing.Size(352, 21);
      this.comboBoxEvents.TabIndex = 2;
      // 
      // listViewEventMap
      // 
      this.listViewEventMap.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.listViewEventMap.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeaderEvent,
            this.columnHeaderEventCommand});
      this.listViewEventMap.ContextMenuStrip = this.contextMenuStripEvents;
      this.listViewEventMap.FullRowSelect = true;
      this.listViewEventMap.GridLines = true;
      this.listViewEventMap.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
      this.listViewEventMap.HideSelection = false;
      this.listViewEventMap.Location = new System.Drawing.Point(8, 8);
      this.listViewEventMap.Name = "listViewEventMap";
      this.listViewEventMap.Size = new System.Drawing.Size(504, 296);
      this.listViewEventMap.TabIndex = 0;
      this.listViewEventMap.UseCompatibleStateImageBehavior = false;
      this.listViewEventMap.View = System.Windows.Forms.View.Details;
      this.listViewEventMap.DoubleClick += new System.EventHandler(this.listViewEventMap_DoubleClick);
      this.listViewEventMap.KeyDown += new System.Windows.Forms.KeyEventHandler(this.listViewEventMap_KeyDown);
      // 
      // columnHeaderEvent
      // 
      this.columnHeaderEvent.Text = "Event";
      this.columnHeaderEvent.Width = 200;
      // 
      // columnHeaderEventCommand
      // 
      this.columnHeaderEventCommand.Text = "Command";
      this.columnHeaderEventCommand.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
      this.columnHeaderEventCommand.Width = 280;
      // 
      // contextMenuStripEvents
      // 
      this.contextMenuStripEvents.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.removeEventToolStripMenuItem});
      this.contextMenuStripEvents.Name = "contextMenuStripEvents";
      this.contextMenuStripEvents.Size = new System.Drawing.Size(125, 26);
      // 
      // removeEventToolStripMenuItem
      // 
      this.removeEventToolStripMenuItem.Image = global::Translator.Properties.Resources.Delete;
      this.removeEventToolStripMenuItem.Name = "removeEventToolStripMenuItem";
      this.removeEventToolStripMenuItem.Size = new System.Drawing.Size(124, 22);
      this.removeEventToolStripMenuItem.Text = "&Remove";
      this.removeEventToolStripMenuItem.Click += new System.EventHandler(this.removeEventToolStripMenuItem_Click);
      // 
      // tabPageMacro
      // 
      this.tabPageMacro.Controls.Add(this.listViewMacro);
      this.tabPageMacro.Controls.Add(this.buttonTestMacro);
      this.tabPageMacro.Controls.Add(this.buttonDeleteMacro);
      this.tabPageMacro.Controls.Add(this.buttonNewMacro);
      this.tabPageMacro.Controls.Add(this.buttonEditMacro);
      this.tabPageMacro.Location = new System.Drawing.Point(4, 22);
      this.tabPageMacro.Name = "tabPageMacro";
      this.tabPageMacro.Padding = new System.Windows.Forms.Padding(3);
      this.tabPageMacro.Size = new System.Drawing.Size(520, 374);
      this.tabPageMacro.TabIndex = 2;
      this.tabPageMacro.Text = "Macros";
      this.tabPageMacro.UseVisualStyleBackColor = true;
      // 
      // listViewMacro
      // 
      this.listViewMacro.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.listViewMacro.FullRowSelect = true;
      this.listViewMacro.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None;
      this.listViewMacro.HideSelection = false;
      this.listViewMacro.LabelEdit = true;
      this.listViewMacro.Location = new System.Drawing.Point(8, 8);
      this.listViewMacro.MultiSelect = false;
      this.listViewMacro.Name = "listViewMacro";
      this.listViewMacro.Size = new System.Drawing.Size(504, 328);
      this.listViewMacro.TabIndex = 5;
      this.listViewMacro.UseCompatibleStateImageBehavior = false;
      this.listViewMacro.View = System.Windows.Forms.View.List;
      this.listViewMacro.DoubleClick += new System.EventHandler(this.listViewMacro_DoubleClick);
      this.listViewMacro.AfterLabelEdit += new System.Windows.Forms.LabelEditEventHandler(this.listViewMacro_AfterLabelEdit);
      // 
      // buttonTestMacro
      // 
      this.buttonTestMacro.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonTestMacro.Location = new System.Drawing.Point(208, 344);
      this.buttonTestMacro.Name = "buttonTestMacro";
      this.buttonTestMacro.Size = new System.Drawing.Size(56, 24);
      this.buttonTestMacro.TabIndex = 4;
      this.buttonTestMacro.Text = "Test";
      this.toolTip.SetToolTip(this.buttonTestMacro, "Test the currently selected Macro");
      this.buttonTestMacro.UseVisualStyleBackColor = true;
      this.buttonTestMacro.Click += new System.EventHandler(this.buttonTestMacro_Click);
      // 
      // buttonDeleteMacro
      // 
      this.buttonDeleteMacro.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonDeleteMacro.Location = new System.Drawing.Point(136, 344);
      this.buttonDeleteMacro.Name = "buttonDeleteMacro";
      this.buttonDeleteMacro.Size = new System.Drawing.Size(56, 24);
      this.buttonDeleteMacro.TabIndex = 3;
      this.buttonDeleteMacro.Text = "Delete";
      this.toolTip.SetToolTip(this.buttonDeleteMacro, "Delete the currently selected Macro");
      this.buttonDeleteMacro.UseVisualStyleBackColor = true;
      this.buttonDeleteMacro.Click += new System.EventHandler(this.buttonDeleteMacro_Click);
      // 
      // buttonNewMacro
      // 
      this.buttonNewMacro.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonNewMacro.Location = new System.Drawing.Point(8, 344);
      this.buttonNewMacro.Name = "buttonNewMacro";
      this.buttonNewMacro.Size = new System.Drawing.Size(56, 24);
      this.buttonNewMacro.TabIndex = 1;
      this.buttonNewMacro.Text = "New";
      this.toolTip.SetToolTip(this.buttonNewMacro, "Create a new Macro");
      this.buttonNewMacro.UseVisualStyleBackColor = true;
      this.buttonNewMacro.Click += new System.EventHandler(this.buttonNewMacro_Click);
      // 
      // buttonEditMacro
      // 
      this.buttonEditMacro.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonEditMacro.Location = new System.Drawing.Point(72, 344);
      this.buttonEditMacro.Name = "buttonEditMacro";
      this.buttonEditMacro.Size = new System.Drawing.Size(56, 24);
      this.buttonEditMacro.TabIndex = 2;
      this.buttonEditMacro.Text = "Edit";
      this.toolTip.SetToolTip(this.buttonEditMacro, "Edit the currently selected Macro");
      this.buttonEditMacro.UseVisualStyleBackColor = true;
      this.buttonEditMacro.Click += new System.EventHandler(this.buttonEditMacro_Click);
      // 
      // tabPageIRCommands
      // 
      this.tabPageIRCommands.Controls.Add(this.listViewIR);
      this.tabPageIRCommands.Controls.Add(this.buttonNewIR);
      this.tabPageIRCommands.Controls.Add(this.buttonEditIR);
      this.tabPageIRCommands.Controls.Add(this.buttonDeleteIR);
      this.tabPageIRCommands.Location = new System.Drawing.Point(4, 22);
      this.tabPageIRCommands.Name = "tabPageIRCommands";
      this.tabPageIRCommands.Padding = new System.Windows.Forms.Padding(3);
      this.tabPageIRCommands.Size = new System.Drawing.Size(520, 374);
      this.tabPageIRCommands.TabIndex = 3;
      this.tabPageIRCommands.Text = "IR Commands";
      this.tabPageIRCommands.UseVisualStyleBackColor = true;
      // 
      // listViewIR
      // 
      this.listViewIR.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.listViewIR.FullRowSelect = true;
      this.listViewIR.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None;
      this.listViewIR.HideSelection = false;
      this.listViewIR.LabelEdit = true;
      this.listViewIR.Location = new System.Drawing.Point(8, 8);
      this.listViewIR.MultiSelect = false;
      this.listViewIR.Name = "listViewIR";
      this.listViewIR.Size = new System.Drawing.Size(504, 328);
      this.listViewIR.TabIndex = 0;
      this.listViewIR.UseCompatibleStateImageBehavior = false;
      this.listViewIR.View = System.Windows.Forms.View.List;
      this.listViewIR.DoubleClick += new System.EventHandler(this.listViewIR_DoubleClick);
      this.listViewIR.AfterLabelEdit += new System.Windows.Forms.LabelEditEventHandler(this.listViewIR_AfterLabelEdit);
      // 
      // buttonNewIR
      // 
      this.buttonNewIR.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonNewIR.Location = new System.Drawing.Point(8, 344);
      this.buttonNewIR.Name = "buttonNewIR";
      this.buttonNewIR.Size = new System.Drawing.Size(56, 24);
      this.buttonNewIR.TabIndex = 1;
      this.buttonNewIR.Text = "New";
      this.toolTip.SetToolTip(this.buttonNewIR, "Learn a new IR Command");
      this.buttonNewIR.UseVisualStyleBackColor = true;
      this.buttonNewIR.Click += new System.EventHandler(this.buttonNewIR_Click);
      // 
      // buttonEditIR
      // 
      this.buttonEditIR.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonEditIR.Location = new System.Drawing.Point(72, 344);
      this.buttonEditIR.Name = "buttonEditIR";
      this.buttonEditIR.Size = new System.Drawing.Size(56, 24);
      this.buttonEditIR.TabIndex = 2;
      this.buttonEditIR.Text = "Edit";
      this.toolTip.SetToolTip(this.buttonEditIR, "Re-Learn the currently selected IR Command");
      this.buttonEditIR.UseVisualStyleBackColor = true;
      this.buttonEditIR.Click += new System.EventHandler(this.buttonEditIR_Click);
      // 
      // buttonDeleteIR
      // 
      this.buttonDeleteIR.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.buttonDeleteIR.Location = new System.Drawing.Point(136, 344);
      this.buttonDeleteIR.Name = "buttonDeleteIR";
      this.buttonDeleteIR.Size = new System.Drawing.Size(56, 24);
      this.buttonDeleteIR.TabIndex = 3;
      this.buttonDeleteIR.Text = "Delete";
      this.toolTip.SetToolTip(this.buttonDeleteIR, "Delete the currently selected IR Command");
      this.buttonDeleteIR.UseVisualStyleBackColor = true;
      this.buttonDeleteIR.Click += new System.EventHandler(this.buttonDeleteIR_Click);
      // 
      // checkBoxAutoRun
      // 
      this.checkBoxAutoRun.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.checkBoxAutoRun.AutoSize = true;
      this.checkBoxAutoRun.Location = new System.Drawing.Point(16, 440);
      this.checkBoxAutoRun.Name = "checkBoxAutoRun";
      this.checkBoxAutoRun.Size = new System.Drawing.Size(167, 17);
      this.checkBoxAutoRun.TabIndex = 2;
      this.checkBoxAutoRun.Text = "&Start Translator with Windows";
      this.toolTip.SetToolTip(this.checkBoxAutoRun, "Set this to make Translator automatically start when you turn the computer on");
      this.checkBoxAutoRun.UseVisualStyleBackColor = true;
      this.checkBoxAutoRun.CheckedChanged += new System.EventHandler(this.checkBoxAutoRun_CheckedChanged);
      // 
      // menuStrip
      // 
      this.menuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.configurationToolStripMenuItem,
            this.helpToolStripMenuItem});
      this.menuStrip.Location = new System.Drawing.Point(0, 0);
      this.menuStrip.Name = "menuStrip";
      this.menuStrip.Size = new System.Drawing.Size(544, 24);
      this.menuStrip.TabIndex = 0;
      this.menuStrip.Text = "menuStrip";
      // 
      // configurationToolStripMenuItem
      // 
      this.configurationToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.newToolStripMenuItem,
            this.openToolStripMenuItem,
            this.importToolStripMenuItem,
            this.exportToolStripMenuItem,
            this.toolStripSeparator1,
            this.serverToolStripMenuItem,
            this.toolStripSeparator2,
            this.quitToolStripMenuItem});
      this.configurationToolStripMenuItem.Name = "configurationToolStripMenuItem";
      this.configurationToolStripMenuItem.Size = new System.Drawing.Size(35, 20);
      this.configurationToolStripMenuItem.Text = "&File";
      // 
      // newToolStripMenuItem
      // 
      this.newToolStripMenuItem.Name = "newToolStripMenuItem";
      this.newToolStripMenuItem.Size = new System.Drawing.Size(132, 22);
      this.newToolStripMenuItem.Text = "&New";
      this.newToolStripMenuItem.Click += new System.EventHandler(this.newToolStripMenuItem_Click);
      // 
      // openToolStripMenuItem
      // 
      this.openToolStripMenuItem.Name = "openToolStripMenuItem";
      this.openToolStripMenuItem.Size = new System.Drawing.Size(132, 22);
      this.openToolStripMenuItem.Text = "&Open ...";
      this.openToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
      // 
      // importToolStripMenuItem
      // 
      this.importToolStripMenuItem.Name = "importToolStripMenuItem";
      this.importToolStripMenuItem.Size = new System.Drawing.Size(132, 22);
      this.importToolStripMenuItem.Text = "&Import ...";
      this.importToolStripMenuItem.Click += new System.EventHandler(this.importToolStripMenuItem_Click);
      // 
      // exportToolStripMenuItem
      // 
      this.exportToolStripMenuItem.Name = "exportToolStripMenuItem";
      this.exportToolStripMenuItem.Size = new System.Drawing.Size(132, 22);
      this.exportToolStripMenuItem.Text = "&Export ...";
      this.exportToolStripMenuItem.Click += new System.EventHandler(this.exportToolStripMenuItem_Click);
      // 
      // toolStripSeparator1
      // 
      this.toolStripSeparator1.Name = "toolStripSeparator1";
      this.toolStripSeparator1.Size = new System.Drawing.Size(129, 6);
      // 
      // serverToolStripMenuItem
      // 
      this.serverToolStripMenuItem.Name = "serverToolStripMenuItem";
      this.serverToolStripMenuItem.Size = new System.Drawing.Size(132, 22);
      this.serverToolStripMenuItem.Text = "&Server ...";
      this.serverToolStripMenuItem.Click += new System.EventHandler(this.serverToolStripMenuItem_Click);
      // 
      // toolStripSeparator2
      // 
      this.toolStripSeparator2.Name = "toolStripSeparator2";
      this.toolStripSeparator2.Size = new System.Drawing.Size(129, 6);
      // 
      // quitToolStripMenuItem
      // 
      this.quitToolStripMenuItem.Name = "quitToolStripMenuItem";
      this.quitToolStripMenuItem.Size = new System.Drawing.Size(132, 22);
      this.quitToolStripMenuItem.Text = "&Quit";
      this.quitToolStripMenuItem.Click += new System.EventHandler(this.quitToolStripMenuItem_Click);
      // 
      // helpToolStripMenuItem
      // 
      this.helpToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.translatorHelpToolStripMenuItem,
            this.aboutToolStripMenuItem});
      this.helpToolStripMenuItem.Name = "helpToolStripMenuItem";
      this.helpToolStripMenuItem.Size = new System.Drawing.Size(40, 20);
      this.helpToolStripMenuItem.Text = "&Help";
      // 
      // translatorHelpToolStripMenuItem
      // 
      this.translatorHelpToolStripMenuItem.Name = "translatorHelpToolStripMenuItem";
      this.translatorHelpToolStripMenuItem.Size = new System.Drawing.Size(129, 22);
      this.translatorHelpToolStripMenuItem.Text = "&Contents";
      this.translatorHelpToolStripMenuItem.Click += new System.EventHandler(this.translatorHelpToolStripMenuItem_Click);
      // 
      // aboutToolStripMenuItem
      // 
      this.aboutToolStripMenuItem.Name = "aboutToolStripMenuItem";
      this.aboutToolStripMenuItem.Size = new System.Drawing.Size(129, 22);
      this.aboutToolStripMenuItem.Text = "&About";
      this.aboutToolStripMenuItem.Click += new System.EventHandler(this.aboutToolStripMenuItem_Click);
      // 
      // openFileDialog
      // 
      this.openFileDialog.FileName = "openFileDialog1";
      this.openFileDialog.Filter = "XML Files|*.xml";
      // 
      // saveFileDialog
      // 
      this.saveFileDialog.Filter = "XML Files|*.xml";
      this.saveFileDialog.Title = "Export settings ...";
      // 
      // MainForm
      // 
      this.AcceptButton = this.buttonOK;
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(544, 472);
      this.Controls.Add(this.tabControl);
      this.Controls.Add(this.buttonOK);
      this.Controls.Add(this.checkBoxAutoRun);
      this.Controls.Add(this.menuStrip);
      this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
      this.MainMenuStrip = this.menuStrip;
      this.MinimizeBox = false;
      this.MinimumSize = new System.Drawing.Size(472, 448);
      this.Name = "MainForm";
      this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
      this.Text = "Translator";
      this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainForm_FormClosing);
      this.Load += new System.EventHandler(this.MainForm_Load);
      this.tabControl.ResumeLayout(false);
      this.tabPagePrograms.ResumeLayout(false);
      this.tabPageEvents.ResumeLayout(false);
      this.contextMenuStripEvents.ResumeLayout(false);
      this.tabPageMacro.ResumeLayout(false);
      this.tabPageIRCommands.ResumeLayout(false);
      this.menuStrip.ResumeLayout(false);
      this.menuStrip.PerformLayout();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.ListView listViewButtons;
    private System.Windows.Forms.ColumnHeader columnHeaderButton;
    private System.Windows.Forms.ColumnHeader columnHeaderCommand;
    private System.Windows.Forms.Button buttonNew;
    private System.Windows.Forms.Button buttonModify;
    private System.Windows.Forms.Button buttonDelete;
    private System.Windows.Forms.Button buttonClear;
    private System.Windows.Forms.Button buttonOK;
    private System.Windows.Forms.TabControl tabControl;
    private System.Windows.Forms.TabPage tabPagePrograms;
    private System.Windows.Forms.TabPage tabPageIRCommands;
    private System.Windows.Forms.Button buttonNewIR;
    private System.Windows.Forms.Button buttonEditIR;
    private System.Windows.Forms.Button buttonDeleteIR;
    private System.Windows.Forms.TabPage tabPageMacro;
    private System.Windows.Forms.Button buttonTestMacro;
    private System.Windows.Forms.Button buttonDeleteMacro;
    private System.Windows.Forms.Button buttonNewMacro;
    private System.Windows.Forms.Button buttonEditMacro;
    private System.Windows.Forms.ColumnHeader columnHeaderDescription;
    private System.Windows.Forms.ToolTip toolTip;
    private System.Windows.Forms.CheckBox checkBoxAutoRun;
    private System.Windows.Forms.TabPage tabPageEvents;
    private System.Windows.Forms.Label labelEvent;
    private System.Windows.Forms.ComboBox comboBoxEvents;
    private System.Windows.Forms.ListView listViewEventMap;
    private System.Windows.Forms.ColumnHeader columnHeaderEvent;
    private System.Windows.Forms.ColumnHeader columnHeaderEventCommand;
    private System.Windows.Forms.Button buttonSetCommand;
    private System.Windows.Forms.Button buttonAddEvent;
    private System.Windows.Forms.Label labelCommand;
    private System.Windows.Forms.ComboBox comboBoxCommands;
    private System.Windows.Forms.MenuStrip menuStrip;
    private System.Windows.Forms.ToolStripMenuItem configurationToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem newToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem importToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem exportToolStripMenuItem;
    private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
    private System.Windows.Forms.ToolStripMenuItem quitToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem helpToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem translatorHelpToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem aboutToolStripMenuItem;
    private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
    private System.Windows.Forms.ToolStripMenuItem serverToolStripMenuItem;
    private System.Windows.Forms.OpenFileDialog openFileDialog;
    private System.Windows.Forms.SaveFileDialog saveFileDialog;
    private System.Windows.Forms.ListView listViewIR;
    private System.Windows.Forms.ListView listViewMacro;
    private System.Windows.Forms.ListView listViewPrograms;
    private System.Windows.Forms.ImageList imageListPrograms;
    private System.Windows.Forms.ContextMenuStrip contextMenuStripPrograms;
    private System.Windows.Forms.ContextMenuStrip contextMenuStripEvents;
    private System.Windows.Forms.ToolStripMenuItem removeEventToolStripMenuItem;
  }
}

