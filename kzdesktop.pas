{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in the lazarus           *
 *  directory, for details about the license/copyright.                      *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author....: Raphael Zimermann
  Website...: www.raphaelz.com.br

  About:
    KZDesktop transforms the lazarus IDE to a layout similar to that of the
    Package ViewerPackage Viewer"Delphi Rad Studio" IDE.



-------------------------------------------------------------------------------
    This is a fork of KZDesktop designed to work wirh Lazarus 1.6+
    and is compatible with Dock Solutions like AnchorDockingDesign

    Copyright (C) 2016 Roy Gian Balderrama roygb705@yahoo.com

}

unit kzdesktop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LCLType, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, TypInfo, Buttons, StdCtrls, GraphType, LMessages, MenuIntf, ComCtrls, Menus,
  IniFiles, IDEOptionsIntf, LazIDEIntf;

procedure Register;

  procedure SaveCFG(_Section, _Ident, _Value: String);
  function LoadCFG(_Section, _Ident: String; _Default: String = ''): String;
  function LoadIniFile: TIniFile;

implementation

uses
  KZLazDesktop, uKZConfig;

function KzDesktopCFG: String;
begin
	if LazarusIDE <> nil then begin
		Exit(LazarusIDE.GetPrimaryConfigPath + PathDelim + 'kzdesktop.cfg');
	end;
	Result := ExtractFilePath(Application.ExeName) + PathDelim + 'kzdesktop.cfg';
end;

procedure SaveCFG(_Section, _Ident, _Value: String);
var
  cfg: TIniFile;
begin
  cfg := TIniFile.Create(KzDesktopCFG);
  try
    cfg.WriteString(_Section, _Ident, _Value);
  finally
    FreeAndNil(cfg);
  end;

end;

function LoadCFG(_Section, _Ident: String; _Default: String = ''): String;
var
  cfg: TIniFile;
begin

  cfg := TIniFile.Create(KzDesktopCFG);
  try
    Result := cfg.ReadString(_Section, _Ident, _Default);
  finally
    FreeAndNil(cfg);
  end;

end;

function LoadIniFile: TIniFile;
begin
  Result := TIniFile.Create(KzDesktopCFG);
end;

procedure StartKzDeskTop;
begin
  KZLazDesktopInterface := TKZLazDesktopInterface.Create(Application);
  KZLazDesktopInterface.Execute;
end;

procedure Register;
var
	index: Integer = 1; //be an asshole and put it at the top if finding the last index does not work :)
	optionsRec, subRec: PIDEOptionsGroupRec;
begin
  optionsRec := IDEEditorGroups.GetByIndex(GroupEnvironment);
  if optionsRec <> nil then begin
    subRec := optionsRec^.Items.Last;
	if subRec <> nil then begin
		index := subRec^.Index + 1;
	end else begin
    	index := optionsRec^.Items.Count;
	end;
  end;
  RegisterIDEOptionsEditor(GroupEnvironment, TfmKZConfig, index);
  if (FileExists(KzDesktopCFG)) and (LoadCFG('Initialization', 'Started', 'False') <> 'False') then begin
    StartKzDeskTop;
  end;
end;

end.

///
