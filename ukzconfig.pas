unit uKZConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, StdCtrls, ColorBox, Buttons,

  IDEOptionsIntf, typinfo, variants;

const
  KZCONF_LOCATION_TAB              = 'tab';
  KZCONF_LOCATION_SOURCE_EDITOR    = 'source_editor';
  KZCONF_LOCATION_IDE_BOTTON       = 'ide_bottom';
  KZCONF_LOCATION_OBJECT_INSPECTOR = 'object_inspector';
  KZCONF_ALIGN_LEFT                = 'left';
  KZCONF_ALIGN_RIGHT               = 'right';
  KZCONF_ALIGN_SEPARATE = 'separate';
  KZCONF_ALIGN_NONE = 'none';
  TRUE_STR = 'True';
  FALSE_STR = 'False';

type

  { TKZConfigManager }

  TKZConfigManager = class(TObject)
  private
    FDesignEditor_BkCollor: String;
    FObjectInspector_Align: String;
	FObjectInspector_Align_Orig: String;
	FComponentFind_Align: String;

    FDebugMessageActive: String;
    FStarted: String;

  published
    // Ohoh, as strings ficaram do mesmo tamanho ;) rs
    property ObjectInspector_Align: String read FObjectInspector_Align write FObjectInspector_Align;
	property ComponentFind_Align: String read FComponentFind_Align write FComponentFind_Align;
    property DesignEditor_BkCollor: String read FDesignEditor_BkCollor write FDesignEditor_BkCollor;
    property DebugMessageActive: String read FDebugMessageActive write FDebugMessageActive;
    property Started: String read FStarted write FStarted;

  public
    procedure LoadDefaults;

    procedure Load;
    procedure Save;
    function GetCategory(Category: String): String;

    constructor Create;
  end;

  { TfmKZConfig }

  TfmKZConfig = class(TAbstractIDEOptionsEditor)
    cbDebug: TCheckBox;
    cbMainUse: TCheckBox;
    ColorBox1: TColorBox;
    GroupBox1: TGroupBox;
    rgObjectInspectorPanel: TRadioGroup;
	rgComponentFindPanel: TRadioGroup;
    StaticText1: TStaticText;
  private
    { private declarations }

  public
    { public declarations }

    constructor Create(AOwner: TComponent); override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    destructor Destroy; override;
  end; 

var
  fmKZConfig: TfmKZConfig;
  KZConfigManager: TKZConfigManager;

implementation

uses
  kzdesktop, KZLazDesktop, IniFiles, IDEWindowIntf;

{ TKZConfigManager }

constructor TKZConfigManager.Create;
begin
  inherited Create;
  Load;
end;

procedure TKZConfigManager.LoadDefaults;
begin
  FDesignEditor_BkCollor := AnsiToUtf8(ColorToString(clGray));
  FObjectInspector_Align := KZCONF_ALIGN_LEFT;
  FStarted := 'False';
  FDebugMessageActive := 'False';
end;

procedure TKZConfigManager.Load;
var
  Count, Size, I: Integer;
  List: PPropList;
  PropInfo: PPropInfo;
  cfg: TIniFile;
begin
  Count := GetPropList(Self.ClassInfo, tkAny, nil);
  Size  := Count * SizeOf(Pointer);
  GetMem(List, Size);

  LoadDefaults; // Primeiro, prepara os padrões

  cfg := LoadIniFile;
  try
    Count := GetPropList(Self.ClassInfo, tkProperties, List);

    for I := 0 to Count - 1 do begin
      PropInfo := List^[I];
      SetPropValue(Self, PropInfo^.Name,
          cfg.ReadString(GetCategory(PropInfo^.Name), PropInfo^.Name,
             VarToStr(GetPropValue(Self, PropInfo^.Name))));
    end;
  finally
    FreeMem(List, Size);
    FreeAndNil(cfg);
  end;

end;

procedure TKZConfigManager.Save;
var
  Count, Size, I: Integer;
  List: PPropList;
  PropInfo: PPropInfo;
  cfg: TIniFile;
begin
  Count := GetPropList(Self.ClassInfo, tkAny, nil);
  Size  := Count * SizeOf(Pointer);
  GetMem(List, Size);

  cfg := LoadIniFile;
  try
    Count := GetPropList(Self.ClassInfo, tkProperties, List);

    for I := 0 to Count - 1 do begin
      PropInfo := List^[I];
      cfg.WriteString(GetCategory(PropInfo^.Name), PropInfo^.Name,
          VarToStr(GetPropValue(Self, PropInfo^.Name)));
    end;
    cfg.UpdateFile;
  finally
    FreeMem(List, Size);
    FreeAndNil(cfg);
  end;

end;

function TKZConfigManager.GetCategory(Category: String): String;
begin
  if Category = 'Started' then exit('Initialization');
  if Category = 'DebugMessageActive' then exit('Debug');
  Result := 'Layout';
end;

{ TfmKZConfig }

constructor TfmKZConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //writeln('created');
  try
    KZConfigManager := TKZConfigManager.Create;
    //writeln('created');
  finally
    //writeln('end try created');
  end;
end;

function TfmKZConfig.GetTitle: String;
begin
  Result := 'Min-KZDesktop';
end;

procedure TfmKZConfig.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  // Nada
end;

procedure TfmKZConfig.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  //writeln('will load');
  with KZConfigManager do begin
    //writeln('load');
    Load;

	//if there is a docking manager, object inspector
	//can only work separately (for now)
	//you can combine the inspector via the docking mechanism
	//since the form editor also supports the docking system
	if IDEDockMaster <> nil then begin
		FObjectInspector_Align_Orig := ObjectInspector_Align;
        ObjectInspector_Align := KZCONF_ALIGN_SEPARATE;
		rgObjectInspectorPanel.Enabled := False;
	end;

    if ObjectInspector_Align = KZCONF_ALIGN_SEPARATE then
      rgObjectInspectorPanel.ItemIndex := 0
    else if ObjectInspector_Align = KZCONF_ALIGN_LEFT then
      rgObjectInspectorPanel.ItemIndex := 1
    else
	  rgObjectInspectorPanel.ItemIndex := 2;

    if ComponentFind_Align = KZCONF_ALIGN_NONE then
      rgComponentFindPanel.ItemIndex := 0
    else if ComponentFind_Align = KZCONF_ALIGN_LEFT then
      rgComponentFindPanel.ItemIndex := 1
    else
	  rgComponentFindPanel.ItemIndex := 2;

    cbMainUse.Checked := Started = TRUE_STR;
    cbDebug.Checked := DebugMessageActive = TRUE_STR;

    ColorBox1.Selected := StringToColor(DesignEditor_BkCollor);
  end;
end;

procedure TfmKZConfig.WriteSettings(AOptions: TAbstractIDEOptions);
var
  vChosen: String;
begin

  with KZConfigManager do begin

	if IDEDockMaster <> nil then begin
        ObjectInspector_Align := FObjectInspector_Align_Orig;
	end else begin
	    if rgObjectInspectorPanel.ItemIndex = 0 then
	      ObjectInspector_Align := KZCONF_ALIGN_SEPARATE
	    else if rgObjectInspectorPanel.ItemIndex = 1 then
	      ObjectInspector_Align := KZCONF_ALIGN_LEFT
		else
		  ObjectInspector_Align := KZCONF_ALIGN_RIGHT;
	end;

	if rgComponentFindPanel.ItemIndex = 0 then
		ComponentFind_Align := KZCONF_ALIGN_NONE
	else if rgComponentFindPanel.ItemIndex = 1 then
		ComponentFind_Align := KZCONF_ALIGN_LEFT
	else
		ComponentFind_Align := KZCONF_ALIGN_RIGHT;

    if cbMainUse.Checked then
      Started := TRUE_STR
    else
      Started := FALSE_STR;

    if cbDebug.Checked then
      DebugMessageActive := TRUE_STR
    else
      DebugMessageActive := FALSE_STR;

    DesignEditor_BkCollor := ColorToString(ColorBox1.Selected);

    Save;
  end;

  if Assigned(KZLazDesktopInterface) then
	KZLazDesktopInterface.UpdateFromOptions;
end;

class function TfmKZConfig.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TAbstractIDEEnvironmentOptions;
end;

destructor TfmKZConfig.Destroy;
begin
  KZConfigManager.Free;
  inherited Destroy;
end;

{$R *.lfm}

end.

