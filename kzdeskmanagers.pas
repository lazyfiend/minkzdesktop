unit KZDeskManagers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, LMessages, ComCtrls, ExtCtrls, Dialogs,
  LCLIntf, LCLType, SrcEditorIntf, IDEMsgIntf;

type
  TKZParentType = (kzptParent, kzptParentWindow, kzptDock, kzptNone);

type

  TKZLMessageHandlerEvent = procedure(Sender: TObject; SenderCtrl: TControl; Msg: TLMessage) of object;

  { TKZLMessageHandler }

  TKZLMessageHandler = class(TPersistent)
  private

    Control: TControl;
    InheritedWndMethod: TWndMethod;

    FMessagesIgnore: TList;

    function ValidControl: Boolean;

  public

    OnBefforeMessage,
    OnAfterMessage: TKZLMessageHandlerEvent;

    procedure SelfWndMethod(var TheMessage: TLMessage);
    procedure DestroyMe(Sender: TObject);

    constructor Create(AControl: TControl);

    procedure AddMessageIgnore(LM_: Integer);
    procedure RemoveMessageIgnore(LM_: Integer);

  end;

  {
  TKZDeskManagerFormInfo = class
  public
    Form: TCustomForm;
    MessageHandler: TKZLMessageHandler;
  end;
  }

  { TKZDeskManager }

  TKZDeskManager = class
  private
    function getItems(AIndex: Integer): TCustomForm; // TKZDeskManagerFormInfo;
  protected
    FRemoveBorder: Boolean;

    FormParent: TWinControl;
    ParentType: TKZParentType;

    FItems: TList;

    function IsMessageShowing(LM: TLMessage): Boolean;
  public

    AutoShowHideForms: Boolean;

    property Items[AIndex: Integer]: TCustomForm { TKZDeskManagerFormInfo } read getItems;

    procedure FormParentResize(Sender: TObject);
    procedure FormParentVisibleChange(Sender: TObject);

    procedure Prepare(_Form: TCustomForm); virtual;
    procedure AddForm(_Form: TCustomForm); virtual;
	function CreateHandler(_Form: TCustomForm): TKZLMessageHandler; virtual;
    procedure RemoveForm(_Form: TCustomForm); virtual;
    procedure HideForms; virtual;
    procedure ShowForms; virtual;

    procedure BefforeMessage(Sender: TObject; SenderWindow: TControl; Msg: TLMessage); virtual;
    procedure AfterMessage(Sender: TObject; SenderWindow: TControl; Msg: TLMessage); virtual;

    constructor Create(_ParentControl: TWinControl; _ParentType: TKZParentType);
    destructor Destroy; override;

  end;

  { TKZObjectInspectorManager }

  TKZObjectInspectorManager = class(TKZDeskManager)
  protected
	//this is all designed for "eating" 1 form
	OldBorder: TFormBorderStyle;
    MsgHandler: TKZLMessageHandler;
  public
    procedure Prepare(_Form: TCustomForm); override;
	function CreateHandler(_Form: TCustomForm): TKZLMessageHandler; override;
	procedure RemoveForm(_Form: TCustomForm); override;
  end;

implementation

{ TKZObjectInspectorManager }

procedure TKZObjectInspectorManager.Prepare(_Form: TCustomForm);
var
	isNew: Boolean;
begin
	isNew := FItems.IndexOf(_Form) < 0;
	if isNew then begin
		OldBorder := _Form.BorderStyle;
		_Form.FindChildControl('StatusBar').Visible := False;
	end;

	inherited Prepare(_Form);
	if isNew then begin
		_Form.Align := alClient;
		_Form.Invalidate;
	end;
end;

function TKZObjectInspectorManager.CreateHandler(_Form: TCustomForm): TKZLMessageHandler;
begin
	Result := Inherited CreateHandler(_Form);
	MsgHandler := Result;
end;

procedure TKZObjectInspectorManager.RemoveForm(_Form: TCustomForm);
begin
	if (FItems <> Nil) and (FItems.IndexOf(_Form) >= 0) then begin
		if MsgHandler <> Nil then begin
			MsgHandler.DestroyMe(Nil);
			MsgHandler := Nil;
		end;
		_Form.BorderStyle := OldBorder;
		_Form.FindChildControl('StatusBar').Visible := True;
		_Form.Align := alNone;

		case ParentType of
	    	kzptNone          : ;
	    	kzptDock          : ;
			kzptParent        : _Form.Parent := Nil;
			kzptParentWindow  : _Form.ParentWindow := 0;
	  end;
	end;

	inherited RemoveForm(_Form);
end;

{ TKZLMessageHandler }

function TKZLMessageHandler.ValidControl: Boolean;
begin
  Result := (Assigned(Control)) and (Control <> Nil);
end;

procedure TKZLMessageHandler.SelfWndMethod(var TheMessage: TLMessage);
var
  i: Integer;
begin

  if (not ValidControl) then Exit;

  for i := 0 to FMessagesIgnore.Count - 1 do begin
    if Integer(FMessagesIgnore[i]) = TheMessage.msg then Exit;
  end;

  if Assigned(OnBefforeMessage) then
    OnBefforeMessage(Self, Control, TheMessage);

  if Assigned(InheritedWndMethod) then
    InheritedWndMethod(TheMessage);

  if Assigned(OnAfterMessage) then
    OnAfterMessage(Self, Control, TheMessage);

end;

procedure TKZLMessageHandler.DestroyMe(Sender: TObject);
begin
  Control.WindowProc := InheritedWndMethod;
  Free;
end;

constructor TKZLMessageHandler.Create(AControl: TControl);
begin
  inherited Create;

  FMessagesIgnore := TList.Create;

  Control := AControl;

  InheritedWndMethod :=  Control.WindowProc;
  Control.WindowProc := @SelfWndMethod;

  //Control.AddHandlerOnBeforeDestruction(@DestroyMe);

end;

procedure TKZLMessageHandler.AddMessageIgnore(LM_: Integer);
begin
  FMessagesIgnore.Add(Pointer(LM_));
end;

procedure TKZLMessageHandler.RemoveMessageIgnore(LM_: Integer);
begin
  FMessagesIgnore.Remove(Pointer(LM_));
end;

{ TKZDeskManager }

function TKZDeskManager.getItems(AIndex: Integer): TCustomForm; // TKZDeskManagerFormInfo;
begin
  if FItems = Nil then
    Result := Nil
  else
    Result := TCustomForm(FItems[AIndex]);
end;

function TKZDeskManager.IsMessageShowing(LM: TLMessage): Boolean;
begin
  Result := (LM.msg = LM_SHOWWINDOW) and (TLMShowWindow(LM).Show);
end;

procedure TKZDeskManager.FormParentResize(Sender: TObject);
var
  i: Integer;
begin

  if FItems = Nil then Exit;

  for i := 0 to FItems.Count - 1 do begin
    //if (Items[i].Form <> Nil) and (Assigned(Items[i].Form)) then
    if Assigned( Items[i] ) then begin
      //with Items[i].Form do
      with Items[i] do begin
        Left := 0;
        Top := 0;
        Width  := FormParent.Width;
        Height := FormParent.Height;
      end;

    end;

  end;

end;

procedure TKZDeskManager.FormParentVisibleChange(Sender: TObject);
begin

  if (not AutoShowHideForms) or (FormParent = Nil) then Exit;

  if FormParent.Visible then
    ShowForms
  else
    HideForms;

end;

procedure TKZDeskManager.Prepare(_Form: TCustomForm);
begin
  AddForm(_Form);
end;

procedure TKZDeskManager.AddForm(_Form: TCustomForm);
var
  aRect: TRect;
  //item: TKZDeskManagerFormInfo;
begin
  if FItems.IndexOf(_Form) >= 0 then Exit;

  with _Form do begin

    if Self.FRemoveBorder then
      BorderStyle := bsNone;

    case ParentType of
      kzptNone          : ;

      kzptDock          :
      begin
        aRect.Left := 0;
        aRect.Top := 0;
        aRect.Right := Width;
        aRect.Bottom := Height;
        Dock(FormParent, aRect);
      end;

      kzptParent        : Parent := FormParent;
      kzptParentWindow  : ParentWindow := FormParent.Handle;
    end;

    with CreateHandler(_Form) do begin
      OnBefforeMessage := @BefforeMessage;
      OnAfterMessage := @AfterMessage;
    end;

  end;

  FItems.Add(_Form);
end;

function TKZDeskManager.CreateHandler(_Form: TCustomForm): TKZLMessageHandler;
begin
  Result := TKZLMessageHandler.Create(_Form);
end;

procedure TKZDeskManager.RemoveForm(_Form: TCustomForm);
begin
  if FItems = Nil then Exit;

  FItems.Remove(_Form);
end;

procedure TKZDeskManager.HideForms;
var
  i: Integer;
begin

  if FItems = Nil then Exit;

  for i := 0 to FItems.Count - 1 do begin
    if (Items[i] <> Nil) and (Assigned(Items[i])) then begin
      with Items[i] do begin

        Hide;

      end;
    end;
  end;

end;

procedure TKZDeskManager.ShowForms;
var
  i: Integer;
begin

  if FItems = Nil then Exit;

  for i := 0 to FItems.Count - 1 do begin

    if (Items[i] <> Nil) and (Assigned(Items[i])) then begin
      with Items[i] do begin
        Visible := True;
      end;
    end;
  end;
end;

procedure TKZDeskManager.BefforeMessage(Sender: TObject;
  SenderWindow: TControl; Msg: TLMessage);
begin
  if Msg.msg = LM_DESTROY then begin
    RemoveForm(TCustomForm( SenderWindow ));
  end;
end;

procedure TKZDeskManager.AfterMessage(Sender: TObject; SenderWindow: TControl;
  Msg: TLMessage);
begin
  if IsMessageShowing(Msg) then FormParentResize(FormParent);
end;

constructor TKZDeskManager.Create(_ParentControl: TWinControl;
  _ParentType: TKZParentType);
begin
  FormParent := _ParentControl;
  ParentType := _ParentType;

  FItems := TList.Create;

  FormParent.AddHandlerOnResize(@FormParentResize);
  FormParent.AddHandlerOnVisibleChanged(@FormParentVisibleChange);

  FRemoveBorder := True;
  AutoShowHideForms := False;

end;

destructor TKZDeskManager.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

end.

