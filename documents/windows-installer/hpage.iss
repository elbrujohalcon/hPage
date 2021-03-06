﻿; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{EA29B0BB-6525-416D-9D9D-3EC7C7BF2EB8}
AppName=λPage
AppVerName=λPage 0.10.0
AppPublisher=Fernando Benavides
AppPublisherURL=http://haskell.hpage.com/
AppSupportURL=http://haskell.hpage.com/
AppUpdatesURL=http://haskell.hpage.com/
DefaultDirName={pf}\hpage-0.10.0
DisableDirPage=yes
DefaultGroupName=λPage
AllowNoIcons=yes
LicenseFile=C:\Documents and Settings\Fernando Benavides\My Documents\Projects\hPage\LICENSE
OutputDir=C:\Documents and Settings\Fernando Benavides\My Documents\Projects\hPage\dist\build
OutputBaseFilename=hpage-0.10.0
SetupIconFile=C:\Documents and Settings\Fernando Benavides\My Documents\Projects\hPage\res\images\icon\hpage.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: quicklaunchicon; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Files]
Source: C:\Documents and Settings\Fernando Benavides\Application Data\cabal\bin\hpage.exe; DestDir: {app}; Flags: ignoreversion
Source: C:\Documents and Settings\Fernando Benavides\Application Data\cabal\hpage-0.10.0\*; DestDir: {app}; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: system32\wxmsw28u_gcc_custom.dll; DestDir: {sys}; Attribs: system; Flags: regserver sharedfile 32bit noregerror; Tasks: ; Languages:
Source: system32\mingwm10.dll; DestDir: {sys}; Attribs: system; Flags: regserver sharedfile 32bit noregerror; Tasks: ; Languages:

[Icons]
Name: {group}\λPage; Filename: {app}\hpage.exe; IconFilename: {app}\res\images\icon\hpage.ico
Name: {group}\{cm:ProgramOnTheWeb,λPage}; Filename: http://haskell.hpage.com/
Name: {group}\{cm:UninstallProgram,λPage}; Filename: {uninstallexe}
Name: {commondesktop}\λPage; Filename: {app}\hpage.exe; Tasks: desktopicon; IconFilename: {app}\res\images\icon\hpage.ico
Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\λPage; Filename: {app}\hpage.exe; Tasks: quicklaunchicon; IconFilename: {app}\res\images\icon\hpage.ico

[Run]
Filename: {app}\hpage.exe; Description: {cm:LaunchProgram,λPage}; Flags: nowait postinstall skipifsilent


