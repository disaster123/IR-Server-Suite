!define LANG "ENGLISH" ; Must be the lang name define my NSIS

!insertmacro LANG_STRING ^UninstallLink                      "Uninstall $(^Name)"

!insertmacro LANG_STRING TEXT_MSGBOX_REMOVE_ALL              "Do you want to remove your User settings?$\r$\nAttention: This will remove all your customised settings including Skins and Databases."



# Descriptions for components (sections)
!insertmacro LANG_STRING DESC_SectionInputService        "A windows service that provides access to your IR devices."

!insertmacro LANG_STRING DESC_SectionGroupMP             "MediaPortal plugins."
!insertmacro LANG_STRING DESC_SectionMPControlPlugin     "Connects to the Input Service to control MediaPortal."
!insertmacro LANG_STRING DESC_SectionMPBlastZonePlugin   "Lets you control your IR devices from within the MediaPortal GUI."
!insertmacro LANG_STRING DESC_SectionTV2BlasterPlugin    "For tuning external channels (on Set Top Boxes) with the default MediaPortal TV engine."

!insertmacro LANG_STRING DESC_SectionGroupTV3            "MediaPortal TV Server plugins."
!insertmacro LANG_STRING DESC_SectionTV3BlasterPlugin    "For tuning external channels (on Set Top Boxes) with the MediaPortal TV server."
!insertmacro LANG_STRING DESC_SectionGroupMCE            "Windows Media Center add-ons."

!insertmacro LANG_STRING DESC_SectionDebugClient         "Simple testing tool for troubleshooting input and communications problems."
!insertmacro LANG_STRING DESC_SectionIRFileTool          "Tool for learning, modifying, testing, correcting and converting IR command files."
!insertmacro LANG_STRING DESC_SectionKeyboardInputRelay  "Relays keyboard input to the Input Service to act on keypresses like remote buttons."
!insertmacro LANG_STRING DESC_SectionTranslator          "Control your whole PC."
!insertmacro LANG_STRING DESC_SectionTrayLauncher        "Simple program to launch an application of your choosing when a particular button is pressed."
!insertmacro LANG_STRING DESC_SectionVirtualRemote       "Simulated remote control. Includes PC application, web, and Smart Devices versions."

!insertmacro LANG_STRING DESC_SectionIRBlast             "Command line tools for blasting IR codes."
!insertmacro LANG_STRING DESC_SectionDboxTuner           "Command line tuner for Dreambox devices."
!insertmacro LANG_STRING DESC_SectionHcwPvrTuner         "Command line tuner for Hauppauge PVR devices."
!insertmacro LANG_STRING DESC_SectionMCEBlaster          "For tuning external channels (on Set Top Boxes) with Windows Media Center."

# Strings for �ddRemove-Page
!insertmacro LANG_STRING TEXT_ADDREMOVE_HEADER          "Already Installed"
!insertmacro LANG_STRING TEXT_ADDREMOVE_HEADER2_REPAIR  "Choose the maintenance option to perform."
!insertmacro LANG_STRING TEXT_ADDREMOVE_HEADER2_UPDOWN  "Choose how you want to install $(^Name)."
!insertmacro LANG_STRING TEXT_ADDREMOVE_INFO_REPAIR     "$(^Name) ${VERSION} is already installed. Select the operation you want to perform and click Next to continue."
!insertmacro LANG_STRING TEXT_ADDREMOVE_INFO_UPGRADE    "An older version of $(^Name) is installed on your system. It is recommended that you uninstall the current version before installing. Select the operation you want to perform and click Next to continue."
!insertmacro LANG_STRING TEXT_ADDREMOVE_INFO_DOWNGRADE  "A newer version of $(^Name) is already installed! It is not recommended that you install an older version. If you really want to install this older version, it's better to uninstall the current version first. Select the operation you want to perform and click Next to continue."
!insertmacro LANG_STRING TEXT_ADDREMOVE_REPAIR_OPT1     "Add/Remove/Reinstall components"
!insertmacro LANG_STRING TEXT_ADDREMOVE_REPAIR_OPT2     "Uninstall $(^Name)"
!insertmacro LANG_STRING TEXT_ADDREMOVE_UPDOWN_OPT1     "Uninstall before installing"
!insertmacro LANG_STRING TEXT_ADDREMOVE_UPDOWN_OPT2     "Do not uninstall"

# Strings for ServerServiceMode-Page
!insertmacro LANG_STRING ServerServiceModePage_HEADER    "TEXT_ServerServiceMode_HEADER"
!insertmacro LANG_STRING ServerServiceModePage_HEADER2   "TEXT_ServerServiceMode_HEADER2"
!insertmacro LANG_STRING ServerServiceModePage_INFO      "TEXT_ServerServiceMode_INFO"

!insertmacro LANG_STRING ServerServiceModePage_OPT0      "TEXT_ServerServiceMode_OPT0"
!insertmacro LANG_STRING ServerServiceModePage_OPT0_DESC "TEXT_ServerServiceMode_OPT0_DESC"

!insertmacro LANG_STRING ServerServiceModePage_OPT1      "TEXT_ServerServiceMode_OPT1"
!insertmacro LANG_STRING ServerServiceModePage_OPT1_DESC "TEXT_ServerServiceMode_OPT1_DESC"

