{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in the lazarus           *
 *  directory, for details about the copyright/license.                      *
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
    "Delphi Rad Studio" IDE.

  My public SVN: svn://svn.code.sf.net/p/kzdesktop/code/trunk





  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------

  Notes: [PT-BR]

  Abaixo imformações interessantes, sobre o comportamento do processo
  nos variados sistemas operacionais:

  - Quando colocamos uma janela ou componente dentro de outra (por meio do
    ParentWindow ou SetParent), a janela ou componente fica impossibilitada
    de receber foco. Esse bug é até mesmo reconhecido pela microsoft. Para
    a janela receber foco, seu parent deve receber o foco. Porisso ao chamar
    as janelas que tem um parent, é necessário dar foco neste parent!

  - No windows, quando fazemos o tratamento da posição do form no designer, as linhas
    e posição do TDesigner se perdem; Para resolver este problema, basta executar o
    "UpdateControlState" no form. PORÉM, o UpdateControlState é um protected, e não
    pode ser chamado diretamente. Para ser chamado indiretamente, deve-se usar o
    comando "with...do". Não sei porque isso ocorre, talvez seja erro do compilador,
    mas no fim das contas funciona.

  - Problema com os LMessages? Note que algumas constantes de mensagens são a soma
    de uma mensagem com outra ou com um valor. Nestes casos, usar um 'in' não funciona;
    tornase necessário usar o '=' no lugar, para verificar se a mensagem em questão
    é a mensagem LM_ABC...

  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------




  This is a fork of KZDesktop designed to work wirh Lazarus 1.6+
  and is compatible with Dock Solutions like AnchorDockingDesign

  Copyright (C) 2016 Roy Gian Balderrama roygb705@yahoo.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit KZLazDesktop;

{$mode objfpc}{$H+}

//{$DEFINE MODO_TESTE}
//{$DEFINE TESTE2}
//{$DEFINE TESTE_MSGS}

interface

uses
  Classes, SysUtils, Math,
  Forms, WSForms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, LMessages, contnrs,

  MenuIntf, SrcEditorIntf, ObjectInspector, IDEMsgIntf, ProjectIntf,
  LazIDEIntf, IDEWindowIntf, ComponentEditors, FormEditingIntf,
  ComponentReg, IDEOptionsIntf,

  KZDeskManagers, process, LCLIntf, LCLType, LCLClasses,
  intfgraphics, StdCtrls, Buttons, EditBtn, Menus,
  dateutils, types, typinfo, strutils;

const
  PreviewFactor: Single = 7;

  KZ_USE_KZTOOLS        = False;
  KZ_DOCK_FORM_DESIGN   = True;
  KZ_REMOVE_FORM_BORDER = False;

  KZ_OBJECT_INSPECTOR_PARENT_TYPE : TKZParentType = kzptParent;        // Default: kzptParent
  KZ_FORM_DESIGNER_PARENT_TYPE    : TKZParentType = kzptParentWindow;  // Default: kzptParentWindow

type

  { TDesignPanel }

  TDesignPanel = class(TPanel)
  protected
    FLoadingForm: Boolean;

    FFakeWindow: TPanel;
    FFakeWindow_MouseDown: Boolean;
    FFakeWindow_MousePos: TPoint;
    FFakeWindowCaption: TPanel;

    //FDefaultClientOrigin: TPoint;
    //FDefaultClientOriginPrepared: Boolean;

    procedure InitializeWnd; override;

    procedure SetupScreen;
    //procedure PrepareFormsOrigin;

    function FormIsValid(Form: TCustomForm): Boolean;
    procedure HideWindowDesign(Form: TCustomForm);
    function IsFormDesign(fm: TCustomForm): boolean;
    function LoadedFormIsValid: Boolean;
    function FormZeroOrigin(Form: TCustomForm): TPoint;

    procedure AddForm(Form: TCustomForm);
    procedure UnLoadForm(Form: TCustomForm);

  public
    FExecuteBefforeMessage: Boolean;

    LoadedForm,
    FLastForm: TCustomForm;
    FFormList: TFPObjectList;
    OnFormLoad,
    OnLoadedFormChangeBounds: TNotifyEvent;

    procedure Screen_FormAdded(Sender: TObject; Form: TCustomForm);

	procedure Form_InitWindow(SenderForm: TControl);
    procedure Form_BefforeMessage(Sender: TObject; SenderForm: TControl; Msg: TLMessage);
    procedure Form_AfterMessage(Sender: TObject; SenderForm: TControl; Msg: TLMessage);

    procedure FFakeWindowMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FFakeWindowMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FFakeWindowMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FFakeWindowResize(Sender: TObject);
    procedure FFakeWindowCaptionPaint(Sender: TObject);

  published
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure UnLoadLoadedForm;
    procedure LoadLastForm;
    procedure UpdateScreenForms;
  end;

  { TFindComponentButton }

  TFindComponentButton = class(TSpeedButton)
  private
    ComponentButton: TSpeedButton;
    ComponentPage: TTabSheet;
    findEdt: TCustomEditButton;
    FClassC: String;

    Comp: TRegisteredComponent;

  public
    procedure Click; override;
    procedure Prepare(_Comp: TRegisteredComponent);
  end;

  { TTabComponentSearch }

  TTabComponentSearch = class(TPanel)
  public
    MyLabel: TLabel;
    MyPanel: TPanel;
    procedure DestroyButtons;
    function NewButton: TFindComponentButton;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TKZLazDesktopInterface }

  TKZLazDesktopInterface = class(TForm)
    edFindComponent: TEditButton;
    imgDesignAreaView: TPaintBox;
    imgWindowScreenPreview: TImage;
    Label1: TLabel;
	pnlObjectInspector: TPanel;
    pnDesignAreaView: TPanel;
    pnCaptionWindowScreenDesignAreaView: TPanel;
    pnScreenMove: TPanel;
    pnFindComponent: TPanel;
    pnCaptionWindowScreenPreview: TPanel;
    pnWindowScreenPreview: TPanel;
    pnScreenPreview: TPanel;
    pnComponents: TPanel;
    pnDesign: TPanel;
    pnCorpo: TPanel;
    pnWindowScreenDesignAreaView: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
	spObjectInspector: TSplitter;
    procedure edFindComponentButtonClick(Sender: TObject);
    procedure edFindComponentChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pnScreenMovePanelResize;
    procedure imgDesignAreaViewPaint(Sender: TObject);
    procedure imgWindowScreenPreviewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure imgWindowScreenPreviewMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: integer);
    procedure imgWindowScreenPreviewMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure pnDesignAreaViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnDesignAreaViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnDesignAreaViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }

    FPreviewMoving: boolean;
    FPreviewMovingPoint: TPoint;

    FDesignAreaViewMoving: boolean;
    FDesignAreaViewMovingPoint: TPoint;

    FDesignPanel: TDesignPanel;

    procedure MainIDEFormShow(TheOwner: TObject);

    function PixelPerc(vl, perc: single): integer;

  private
    { private 'Is' }

    function IsFormDesign(fm: TCustomForm): boolean;
	function IsObjectInspector(fm: TCustomForm): boolean;

  private
    { private objects }

    TabComponentSearch: TTabComponentSearch;
    PackageListHandles: TList;

    function MainIDEForm: TCustomForm;
	function ObjectInspectorDlg: TObjectInspectorDlg;

    function pnlSpeedButtons: TPanel;
    function pnlRightSpeedButtons: TPanel;

  private
    { Private Managers }

	ObjectInspectorManager: TKZObjectInspectorManager;
	ObjInspector_OldShow: TNotifyEvent;
	ObjInspector_OldHide: TNotifyEvent;
  public
    { public setups }

    FEnterInEditor: boolean;

    procedure SetupPreviewFormDesign(_Form: TCustomForm);
    procedure SetupFormDesignAreaView(_Form: TCustomForm);

    procedure OnIDERestoreWindows(Sender: TObject);
	procedure ObjInspector_OnShow(Sender: TObject);
	procedure ObjInspector_OnHide(Sender: TObject);
    function OnProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    function ComponentPageControl: TPageControl;

    procedure DesignPanel_OnLoadForm(Sender: TObject);
    procedure DesignPanel_OnLoadedFormChangeBounds(Sender: TObject);

    procedure SetupScreen;
    procedure SetupDefaultBars;

	procedure ActivateDesigner(Sender: TObject; AEditor: TSourceEditorInterface;
		AComponentPaletteClassSelected: Boolean);

  public
    { public declarations }

    procedure UpdateFromOptions;

    procedure Execute;

  end;

  procedure AddMessageDebug(_Msg: String);


var
  KZLazDesktopInterface: TKZLazDesktopInterface;

implementation

uses
  kzdesktop, uKZConfig;

procedure AddMessageDebug(_Msg: String);
begin
	try
		if (KZConfigManager.DebugMessageActive = 'True') and (IDEMessagesWindow <> Nil) then
			IDEMessagesWindow.AddMsg(FormatDateTime('HH:mm:ss', Now) + ' ' + _Msg, '', 0);
	except on e: Exception do begin
			try
			if (KZConfigManager.DebugMessageActive = 'True') then writeln(_Msg);
			except on e: Exception do ;
			end;
		end;
	end;
end;

function IsFormDesignClass_Internal(fm: TCustomForm): boolean;
begin
	if not Assigned(fm) then Exit(False);
	Result := (fm.ClassName = 'TNonControlDesignerForm') or (fm.ClassName = 'TFrameDesignerForm');
    if not Result then begin
	  	Result := (fm.ClassType.GetInterfaceEntry(IFrameDesigner)<>Nil) or
	  		((Pos('DesignerForm',fm.ClassName) > 0) and
	  		((fm.ClassType.GetInterfaceEntry(INonControlDesigner)<>Nil) or
	  		(fm.ClassType.GetInterfaceEntry(INonFormDesigner)<>Nil)));
    end;
end;

function IsFormDesign_Internal(fm: TCustomForm): boolean;
begin
	Result := Assigned(fm) and ((csDesignInstance in fm.ComponentState) or
		(IsFormDesignClass_Internal(fm)));
end;


{ TDesignPanel }

procedure TDesignPanel.SetupScreen;
begin
  Screen.AddHandlerFormAdded(@Screen_FormAdded);
end;

function TDesignPanel.FormIsValid(Form: TCustomForm): Boolean;
begin
  Result := Assigned(Form);
end;

procedure TDesignPanel.HideWindowDesign(Form: TCustomForm);
begin
  if (not Assigned(Form)) then Exit;
  try
    ShowWindow(Form.Handle, SW_HIDE);
  except on e: Exception do ;
  end;
end;

function TDesignPanel.IsFormDesign(fm: TCustomForm): boolean;
begin
  Result := IsFormDesign_Internal(fm);
end;

function TDesignPanel.LoadedFormIsValid: Boolean;
begin
  Result := FormIsValid(LoadedForm);
end;

function TDesignPanel.FormZeroOrigin(Form: TCustomForm): TPoint;
var
  w: TWinControl;
  rc1, rc2: TRect;
  i: Integer;
begin
  if not KZ_REMOVE_FORM_BORDER then begin
    Result.x := 0;
    Result.y := 0;
    Exit;
  end;

  if FormIsValid(Form) then begin
    try
      w := TWinControl.Create(Self); // Tem que ser em Self se não dá pau!
      w.Left := 0;
      w.Top := 0;
      w.Parent := Form;

      GetWindowRect(w.Handle, rc1);
      GetWindowRect(Form.Handle, rc2);

      // Uso o 'Max' para evitar um rect negativo (acontece no ubuntu)
      Result.x := Max(rc1.Left - rc2.Left, 0);
      Result.y := Max(rc1.Top  - rc2.Top,  0);

      // Importante: Se tiver um menu no form, precisa colocar essa diferença!
      for i := 0 to Form.ComponentCount - 1 do begin
        if Form.Components[i] is TMainMenu then begin
          Result.y := Result.y - GetSystemMetrics(SM_CYMENU);
        end;
      end;
    finally
      FreeAndNil(w);
    end;
  end else begin
    Result.x := 0;
    Result.y := 0;
  end;
end;

procedure TDesignPanel.Form_AfterMessage(Sender: TObject; SenderForm: TControl;
  Msg: TLMessage);
var
  pRect: TRect;

  procedure DockFakeWindow(Form: TCustomForm);
  begin
    with FFakeWindow do begin
      Visible := ((Form <> Nil) and (Assigned(Form)));
      if Visible then
      begin
        Left    := ((Form.Left) - (FFakeWindowCaption.Left)) + FormZeroOrigin(Form).x;
        Top     := ((Form.Top) - (FFakeWindowCaption.Top + FFakeWindowCaption.Height)) + FormZeroOrigin(Form).y;

        Width   := Form.Width + (FFakeWindowCaption.Left * 2);
        Height  := Form.Height + (FFakeWindowCaption.Top + FFakeWindowCaption.Height) + FFakeWindowCaption.Top;
      end;

      FFakeWindowResize(FFakeWindow); // Força o resize, que não acontece se
                                      // tudo já estiver na mesma posição...
    end;
  end;

  procedure FocusFakeWindow;
  begin
    if FFakeWindow.Visible then
    begin
      Self.Left := (-FFakeWindow.Left) + 5;
      Self.Top  := (-FFakeWindow.Top)  + 5;
    end else if LoadedFormIsValid then begin
      Self.Left := (-LoadedForm.Left) + 5;
      Self.Top  := (-LoadedForm.Top)  + 5;
    end;
  end;

  procedure _UpdateFormBorder(Form: TCustomForm);
  var
    rgn: HRGN;
  begin
    if not KZ_REMOVE_FORM_BORDER then Exit;

    with Form, FormZeroOrigin(Form) do begin

      // Tira a borda do form. Esta é a melhor maneira de se fazer isso,
      // pois não prejudica as posições e tamanhos. ;)
      rgn := CreateRectRgn(
        x,
        y,
        x + Width,
        y + Height);

      SetWindowRgn(Handle, rgn, True);
    end;
  end;

  procedure _UpdateFormDesigner(Form: TCustomForm);
  begin
    // Obs: Tem que ser feito com o "do", conforme
    // observação no cabeçário da fonte...
    with TCustomForm( Form ) do UpdateControlState;
  end;

begin
  // Se a tela foi exibida...
  if (Msg.msg = LM_SHOWWINDOW) then begin

    // Se realmente está mostrando...
    if TLMShowWindow(Msg).Show then begin

      // Se o formulário é o formulário carregado...
      if LoadedFormIsValid and (TCustomForm(SenderForm) = LoadedForm) then begin

        // Ajusta o FakeWindow
        DockFakeWindow(LoadedForm);

        // Atualiza a borda do formulário
        _UpdateFormBorder(LoadedForm);

        // Foca o designer no formulário
        FocusFakeWindow;

        // Importante: Seta o foco no Self (Pelo sistema! 'SetFocus' não funfa!)
        LCLIntf.SetFocus(Self.Handle); // Isso é importante pois as janelas modal não trabalham
                                       // o foco; Para que elas estejam em foco, o seu Parent
                                       // deve receber o foco.

        // Atualiza o desenho do Self, pois às vezes dá umas 'zicas'...
        //Self.Update; Self.Parent.Update;

      end;

    end;

    KZLazDesktopInterface.pnScreenMovePanelResize;

  end;

  // Se o formulário foi movido...
  if (Msg.msg = LM_MOVE) then begin

    // Se o formulário é o formulário carregado...
    if LoadedFormIsValid and (TCustomForm(SenderForm) = LoadedForm) then begin

      AddMessageDebug(LoadedForm.Name + format(' after move (a), x=%d ; y=%d',
        [LoadedForm.left, LoadedForm.top]));

      // Ajusta o FakeWindow
      DockFakeWindow(LoadedForm);

      // Atualiza a borda do formulário
      _UpdateFormBorder(LoadedForm);

      // Foca o designer no formulário
      FocusFakeWindow;

      // Atualiza o Designer (para não dar zica na posição do TDesigner)
      _UpdateFormDesigner(LoadedForm);

      // Executa o evento de ao alterar tamanho/posição
      if Assigned(OnLoadedFormChangeBounds) then
        OnLoadedFormChangeBounds(Self);

      AddMessageDebug(LoadedForm.Name + format(' after move (b), x=%d ; y=%d',
          [LoadedForm.left, LoadedForm.top]));

    end;

  end;


  // Se o formulário foi redimensionado...
  if (Msg.msg = LM_SIZE) then begin

    // Se o formulário é o formulário carregado...
    if LoadedFormIsValid and (TCustomForm(SenderForm) = LoadedForm) then begin

      AddMessageDebug(LoadedForm.Name + format(' after size (a), x=%d ; y=%d',
        [LoadedForm.left, LoadedForm.top]));

      // Ajusta o FakeWindow
      DockFakeWindow(LoadedForm);

      // Atualiza a borda do formulário
      _UpdateFormBorder(LoadedForm);

      // Foca o designer no formulário
      FocusFakeWindow;

      // Executa o evento de ao alterar tamanho/posição
      if Assigned(OnLoadedFormChangeBounds) then
        OnLoadedFormChangeBounds(Self);

      AddMessageDebug(LoadedForm.Name + format(' after size (b), x=%d ; y=%d',
          [LoadedForm.left, LoadedForm.top]));

    end;
  end;
end;

procedure TDesignPanel.Form_InitWindow(SenderForm: TControl);
var
    pRect: TRect;
begin
  // Se havia um formulário carregado, descarrega ele
  if LoadedFormIsValid then begin
    UnLoadLoadedForm;
  end;

  // Define o formulário carregado
  LoadedForm := TCustomForm(SenderForm);

  // Se é a primeira vez que ele está sendo carregado...
  if (LoadedForm.ParentWindow <> Self.Handle) then begin
    // Posiciona o painel no espaço real de trabalho (Desktop),
    // para que quando a janela seja movida, a conversão de posições
    // seja perfeita...
    //AdjustSelfPosition(LoadedForm); OBS: Isso não é mais necessário...

    AddMessageDebug(LoadedForm.Name + format(' loaded (a), x=%d ; y=%d',
        [LoadedForm.left, LoadedForm.top]));

    try
      FExecuteBefforeMessage := False; // Quando seta-se o parent no linux, ele
                                       // ele executa o after message de novo,
                                       // causando um erro de invalid point operation,
                                       // pois a memória ainda não está pronta!

      LoadedForm.BeginUpdateBounds; // O 'AdjustSelfPosition' não é mais necessário
                                    // pois iniciar o update de bounds já impede
                                    // que dê zica na posição do form.


      pRect.Left := LoadedForm.Left;
      pRect.Top := LoadedForm.Top;
      pRect.Right := LoadedForm.Left + LoadedForm.Width;
      pRect.Bottom := LoadedForm.Top + LoadedForm.Height;

      // Prende a tela no painel
      case KZ_FORM_DESIGNER_PARENT_TYPE of
        kzptParentWindow  : LoadedForm.ParentWindow := Self.Handle; // Padrão
        kzptParent        : LoadedForm.Parent := Self;
        kzptDock          : LoadedForm.Dock(Self, pRect);
        kzptNone          : ;
      end;

    finally
      LoadedForm.EndUpdateBounds;
      FExecuteBefforeMessage := True;
    end;

    {
    // Guarda o ponto de origem. Isso é importante para todos os forms
    // que forem carregados, pois não há como pegar o ClientOrigin corretamente
    // enquanto ele está processando as messages. Para que serve esse ponto?
    // Para saber a posição do form sem a borda... ;)
    PrepareFormsOrigin;
    }

    AddMessageDebug(LoadedForm.Name + format(' loaded (b), x=%d ; y=%d',
        [LoadedForm.left, LoadedForm.top]));
  end;

  // Tendo lido (primeira vez ou não), executa o OnLoad
  if Assigned(OnFormLoad) then OnFormLoad(Self);

  // Executa o evento de Ao Alterar tamanho/posição (ora pois, isso
  // deve ser excutado na primeira vez também ;) )
  if Assigned(OnLoadedFormChangeBounds) then
    OnLoadedFormChangeBounds(Self);

  // Guarda como o último form carregado
  FLastForm := LoadedForm;
end;

procedure TDesignPanel.Form_BefforeMessage(Sender: TObject;
  SenderForm: TControl; Msg: TLMessage);
var
  isFrame: Boolean;
  tmpRight: Integer;
  procedure AdjustSelfPosition(_Form: TCustomForm);
  begin

    EXIT; // Isso não é mais necessário...

    // Posiciona o painel no espaço real de trabalho (Desktop),
    // para que quando a janela seja movida, a conversão de posições
    // seja perfeita...

    if _Form.ParentWindow <> Self.Handle then begin
      Self.Left := 0;
      Self.Top  := 0;
    end;

    //GetWindowRect(Self.Handle, pRect);

    //Self.Left := -(pRect.Left - Self.Left);
    //Self.Top  := -(pRect.Top - Self.Top);
  end;

begin
  if not FExecuteBefforeMessage then Exit;

  if Msg.msg = LM_DESTROY then begin
    if LoadedFormIsValid then begin
      UnLoadLoadedForm;
    end;
  end;

  if Msg.msg = LM_ACTIVATE then begin
     AddMessageDebug('CM_[BAH]');
  end;

  // Se está exibindo a tela...
  if (Msg.msg = LM_SHOWWINDOW) then begin
    AddMessageDebug('LM_SHOWWINDOW');

    // Se realmente está mostrando...
    if TLMShowWindow(Msg).Show then begin
      Form_InitWindow(SenderForm);
    end;

  end;

  // Se está prestes a mover a tela...
  if (Msg.msg in [LM_MOVE]) then begin

    // Se o formulário é o formulário carregado...
    if LoadedFormIsValid and (TCustomForm(SenderForm) = LoadedForm) then begin

    AddMessageDebug(LoadedForm.Name + format(' beffore move (a), x=%d ; y=%d',
        [LoadedForm.left, LoadedForm.top]));
    end;
  end;

  // Se está prestes a redimensionar a tela...
  if (Msg.msg in [LM_SIZE]) then begin
    // Se o formulário é o formulário carregado...
    if LoadedFormIsValid and (TCustomForm(SenderForm) = LoadedForm) then begin

      AddMessageDebug(LoadedForm.Name + format(' beffore size (a), x=%d ; y=%d',
        [LoadedForm.left, LoadedForm.top]));
    end;
  end;

end;

procedure TDesignPanel.FFakeWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (x >= FFakeWindow.Width - 3) or (y >= FFakeWindow.Height - 3) then begin
    FFakeWindow_MouseDown := True;
    GetCursorPos(FFakeWindow_MousePos);
  end;

end;

procedure TDesignPanel.FFakeWindowMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  minWidth = 100;
  minHeight = 50;
var
  newPos: TPoint;
  frmPoint : TPoint;
  w, h: Integer;
begin
  with TWinControl(Sender) do begin

    if (x >= FFakeWindow.Width - 3) or (y >= FFakeWindow.Height - 3) then
      Cursor := crSizeNWSE
    else
      Cursor := crDefault;

    if FFakeWindow_MouseDown then begin
      GetCursorPos(newPos);

      frmPoint := ScreenToClient(Mouse.CursorPos);

      if frmPoint.X > minWidth then
        Width := frmPoint.X;

      if frmPoint.Y > minHeight then
        Height := frmPoint.Y;

      if LoadedFormIsValid then begin

        w := FFakeWindowCaption.Width; // Width  - 3 - 3;
        h := Height - ((FFakeWindowCaption.Top * 2) + FFakeWindowCaption.Height);

        try
          //LoadedForm.BeginUpdateBounds; Não faça!

          LoadedForm.Width  := w;
          LoadedForm.Height := h;

        finally
          //LoadedForm.EndUpdateBounds; Não faça!
        end;

        if Assigned(OnLoadedFormChangeBounds) then
          OnLoadedFormChangeBounds(Self);

      end;

    end;

  end;

end;

procedure TDesignPanel.FFakeWindowMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FFakeWindow_MouseDown then
  begin
    FFakeWindow_MouseDown := False;
  end;
end;

procedure TDesignPanel.FFakeWindowResize(Sender: TObject);
var
  fk_rgn, fm_rgn, Region, Region2 : HRGN;
  fk_rc, fm_rc: TRect;
begin

  // Faz um furo quadrado no WindowFake, para quando o form ficar atrás dele
  // por causade um bug. Isso não ocorre no windows, mas é bom padronizar
  // e executar em todos os sistemas.

  fk_rc.Left   := 0;
  fk_rc.Top    := 0;
  fk_rc.Right  := FFakeWindow.Width;
  fk_rc.Bottom := FFakeWindow.Height;

  with fm_rc do
  begin
    Left   := FFakeWindowCaption.Left;
    Top    := FFakeWindowCaption.Top + FFakeWindowCaption.Height;
    Right  := Left + FFakeWindowCaption.Width;
    Bottom := FFakeWindow.Height - (FFakeWindowCaption.Top);
  end;

  fk_rgn := CreateRectRgn(fk_rc.Left, fk_rc.Top, fk_rc.Right, fk_rc.Bottom);
  fm_rgn := CreateRectRgn(fm_rc.Left, fm_rc.Top, fm_rc.Right, fm_rc.Bottom);

  CombineRgn(fk_rgn, fk_rgn, fm_rgn, RGN_DIFF);
  SetWindowRgn(FFakeWindow.Handle, fk_rgn, True);
end;

procedure TDesignPanel.FFakeWindowCaptionPaint(Sender: TObject);
begin
  if not LoadedFormIsValid then Exit;

  FFakeWindowCaption.Canvas.Font.Color := clHighlightText;
  FFakeWindowCaption.Canvas.TextOut(3, 0, LoadedForm.Caption);
end;

procedure TDesignPanel.InitializeWnd;
var
  fm: TForm;
begin
  inherited InitializeWnd;

  with FFakeWindow do begin
    // Essas coisas são feitas aqui porque no linux
    // no OnCreate o handle ainda não está pronto!
    //ParentWindow := Self.Handle;
    Parent := Self;
    ParentColor := False;
    Color := clForm;
  end;
end;

procedure TDesignPanel.AddForm(Form: TCustomForm);
begin
  HideWindowDesign(Form);

  if (Form.ParentWindow = Self.Handle) or (Form.Parent = Self) then Exit;

  with TKZLMessageHandler.Create(Form) do begin
    OnBefforeMessage := @Self.Form_BefforeMessage;
    OnAfterMessage   := @Self.Form_AfterMessage;
  end;
end;

procedure TDesignPanel.UnLoadForm(Form: TCustomForm);
begin
  if Form = nil then Exit;

  HideWindowDesign(Form);

  FFakeWindow.Visible := False;
end;

procedure TDesignPanel.Screen_FormAdded(Sender: TObject; Form: TCustomForm);
begin
  // Aqui é a interceptação principal de quando um
  // form é adicionado!
  if KZ_DOCK_FORM_DESIGN and IsFormDesign(Form) then begin
    AddForm(Form);
  end;
end;

constructor TDesignPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  BevelInner := bvNone;
  BevelOuter := bvNone;

  FLoadingForm    := False;

  FExecuteBefforeMessage := True; // Importante!
  //FDefaultClientOriginPrepared := False;

  FFakeWindow := TPanel.Create(Self);
  with FFakeWindow do begin
    //ParentColor := False;
    //Color := clForm; colocado ao inicializar o handle!
    BorderWidth := 3;

    OnMouseDown := @FFakeWindowMouseDown;
    OnMouseMove := @FFakeWindowMouseMove;
    OnMouseUp   := @FFakeWindowMouseUp;
    OnResize    := @FFakeWindowResize;

  end;

  FFakeWindowCaption := TPanel.Create(Self);
  with FFakeWindowCaption do begin
    Parent := FFakeWindow;
    Align := alTop;
    Height := 18;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    ParentColor := False;
    Color := clActiveCaption;
    OnPaint := @FFakeWindowCaptionPaint;
  end;

  Width := Screen.Width * 2;
  Height := Screen.Height * 2;

  Left := 0;
  Top  := 0;

  SetupScreen;
  FFormList := TFPObjectList.Create();
end;

destructor TDesignPanel.Destroy;
begin
  FreeAndNil(FFakeWindowCaption);
  FreeAndNil(FFakeWindow);
  FreeAndNil(FFormList);
  inherited Destroy;
end;

procedure TDesignPanel.UnLoadLoadedForm;
begin

  if not LoadedFormIsValid then Exit;

  try
    FLoadingForm := True; // Important!
    UnLoadForm(LoadedForm);
    LoadedForm := Nil;
  finally
    FLoadingForm := False;
  end;

end;

procedure TDesignPanel.LoadLastForm;
begin
  if (FLastForm <> Nil) and (Assigned(FLastForm)) then begin
    //LoadForm(FLastForm);
    ShowWindow(FLastForm.Handle, SW_SHOW); // Assim funcioma melhor ;)
  end;

end;

procedure TDesignPanel.UpdateScreenForms;
var
  i: Integer;
begin

  // Esse procedimento é para sistemas em que o self
  // é preparado depois dos forms dsigners serem
  // adicionados à screen

  if not KZ_DOCK_FORM_DESIGN then Exit;

  for i := 0 to Screen.CustomFormCount -1 do begin
    if IsFormDesign(Screen.CustomForms[i]) then
      AddForm(Screen.CustomForms[i]);
  end;

end;

{ TTabComponentSearch }

procedure TTabComponentSearch.DestroyButtons;
begin
  while MyPanel.ControlCount > 0 do begin
    MyPanel.Controls[0].Free;
  end;

end;

function TTabComponentSearch.NewButton: TFindComponentButton;
begin
  Result := TFindComponentButton.Create(Self);
  Result.Parent := MyPanel;
end;

constructor TTabComponentSearch.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  MyLabel := TLabel.Create(Self);
  MyLabel.Parent := Self;
  MyLabel.Align := alTop;
  MyLabel.Font.Style := [fsBold];
  MyLabel.Caption := ' Search results';

  MyPanel := TPanel.Create(Self);
  MyPanel.Parent := Self;
  MyPanel.Align := alClient;
  MyPanel.BevelInner := bvNone;
  MyPanel.BevelOuter := bvNone;

  ShowHint := True;
  Align := alClient;
  Visible := False;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TTabComponentSearch.Destroy;
begin
  DestroyButtons;

  FreeAndNil(MyLabel);
  FreeAndNil(MyPanel);

  inherited Destroy;
end;

{ TFindComponentButton }

procedure TFindComponentButton.Click;
var
  _OnChange: TNotifyEvent;
begin
  inherited Click;

  if Comp <> Nil then begin
    if (Parent <> nil) and (Parent.Parent <> nil) then
    with TTabComponentSearch(Parent.Parent) do begin
      Visible := False;
    end;

    ComponentReg.IDEComponentPalette.Selected := Comp;

    EXIT;
  end;

  if (Parent <> nil) and (Parent.Parent <> nil) then
    with TTabComponentSearch(Parent.Parent) do begin
      Visible := False;
    end;

  ComponentPage.Show;
  ComponentPage.SetFocus;

  with findEdt do begin
    try
      _OnChange := OnChange;
      OnChange := nil;
      Clear;
    finally
      OnChange := _OnChange;
    end;
  end;

  ComponentReg.IDEComponentPalette.Selected :=
    ComponentReg.IDEComponentPalette.FindComponent(FClassC);

end;

procedure TFindComponentButton.Prepare(_Comp: TRegisteredComponent);
var
  i, j, k: Integer;
  tpc: TPageControl;
  oldComp: TRegisteredComponent;
begin
  Comp := _Comp;
  ShowHint := True;
  Flat := True;
  GroupIndex := 1;
  Align := alLeft;
  oldComp := ComponentReg.IDEComponentPalette.Selected;
  try
	  ComponentReg.IDEComponentPalette.Selected := Comp;
    tpc := KZLazDesktopInterface.ComponentPageControl;
    if tpc <> nil then begin
      for i := 0 to tpc.PageCount-1 do begin
        for j := 0 to tpc.Page[i].ControlCount-1 do begin
          if tpc.Page[i].Controls[j].ClassName = 'TScrollBox' then with TScrollBox(tpc.Page[i].Controls[j]) do begin
            for k := 0 to ControlCount-1 do begin
              if AnsiEndsStr(Comp.ComponentClass.ClassName,Controls[k].Name) then begin
                Self.Glyph.Assign(TSpeedButton(Controls[k]).Glyph);
                Self.Width := TSpeedButton(Controls[k]).Width;
                Self.Hint := TSpeedButton(Controls[k]).Hint;
                Exit;
              end;
			end;
		  end;
		end;
       end;
    end;
	  Visible:=False;
  finally
     ComponentReg.IDEComponentPalette.Selected := oldComp;
  end;
end;

{$R *.lfm}

{ TKZLazDesktopInterface }

procedure TKZLazDesktopInterface.FormCreate(Sender: TObject);
var
  tmpRight: Integer;
begin

  with pnScreenPreview do begin
    tmpRight := Left + Width;
    Width := PixelPerc(Screen.Width, PreviewFactor);
    Height := PixelPerc(Screen.Height, PreviewFactor);
    Left := tmpRight - Width;
    DoubleBuffered := True;
  end;

  pnScreenMovePanelResize;

  KZConfigManager := TKZConfigManager.Create;

  FDesignPanel := TDesignPanel.Create(Self);
  with FDesignPanel do begin
    Parent := pnDesign;
    Left := 0;
    Top := 0;
    Width := Screen.Width * 2;
    Height := Screen.Height * 2;
    BevelOuter := bvNone;
    OnFormLoad := @DesignPanel_OnLoadForm;
    OnLoadedFormChangeBounds := @DesignPanel_OnLoadedFormChangeBounds;
  end;

  if KZConfigManager.ObjectInspector_Align <> KZCONF_ALIGN_SEPARATE then
  	pnlObjectInspector.Width := PixelPerc(Screen.Width, 25);
end;

procedure TKZLazDesktopInterface.FormShow(Sender: TObject);
begin
	if KZConfigManager.ObjectInspector_Align <> KZCONF_ALIGN_SEPARATE then
		pnlObjectInspector.Width := StrToInt( LoadCFG('Inspectors', 'Width', IntToStr(pnlObjectInspector.Width)) );

	if IDEDockMaster = Nil then begin
		Self.Width := StrToInt(LoadCFG('FormEditor', 'Width', IntToStr(Self.Width)));
		Self.Height := StrToInt(LoadCFG('FormEditor', 'Height', IntToStr(Self.Height)));
		Self.Top := StrToInt(LoadCFG('FormEditor', 'Top', IntToStr(Self.Top)));
		Self.Left := StrToInt(LoadCFG('FormEditor', 'Left', IntToStr(Self.Left)));
	end;
end;

procedure TKZLazDesktopInterface.pnScreenMovePanelResize;
var
  tmpRight: Integer;
  isFrame: Boolean;
begin
  try
    isFrame := FDesignPanel.LoadedFormIsValid and IsFormDesignClass_Internal(FDesignPanel.LoadedForm);

    with pnScreenMove do begin
      tmpRight := Left + Width;
      Width := PixelPerc(Screen.Width, PreviewFactor);
      if isFrame then
        Height := PixelPerc(Screen.Height, PreviewFactor) + 50
      else
        Height := PixelPerc(Screen.Height, PreviewFactor);
      Left := tmpRight - Width;
      DoubleBuffered := True;
    end;

	SpeedButton1.Visible:= isFrame;
    SpeedButton2.Visible:= isFrame;
    SpeedButton3.Visible:= isFrame;
    SpeedButton4.Visible:= isFrame;
    if isFrame then begin
      SpeedButton1.Top := pnScreenMove.Height - 44;
      SpeedButton1.Left := pnScreenMove.Width - 44;
      SpeedButton2.Top := pnScreenMove.Height - 22;
	  SpeedButton2.Left := pnScreenMove.Width - 44;
 	  SpeedButton3.Top := pnScreenMove.Height - 22;
 	  SpeedButton3.Left := pnScreenMove.Width - 66;
 	  SpeedButton4.Top := pnScreenMove.Height - 22;
 	  SpeedButton4.Left := pnScreenMove.Width - 22;
    end;
  except
    on e: Exception do ; //ignore
	end;
end;

procedure TKZLazDesktopInterface.imgDesignAreaViewPaint(Sender: TObject);
var
  x, y: Integer;
  ARect: TRect;
begin

  with imgDesignAreaView do begin

    if Assigned( Canvas ) and
      (Self.Visible) and
      (Canvas.HandleAllocated)
    then begin
      {
      pnWindowScreenDesignAreaView.PaintTo(   // Isso seria o ideal, mas não funciona em todos os sistemas :(
        Canvas,
        (-Parent.Left) + 3,
        (-Parent.Top) + 3);
      }

      // Desenha o form
      x := (-Parent.Left) + 3;
      y := (-Parent.Top) + 3;

      Canvas.Brush.Color := pnWindowScreenDesignAreaView.Color;
      Canvas.Pen.Color := pnWindowScreenDesignAreaView.Color;
      Canvas.Rectangle(
        x,
        y,
        x + pnWindowScreenDesignAreaView.Width,
        y + pnWindowScreenDesignAreaView.Height);


      // Desenha o caption do form
      Canvas.Brush.Color := pnCaptionWindowScreenDesignAreaView.Color;
      Canvas.Pen.Color := pnCaptionWindowScreenDesignAreaView.Color;
      Canvas.Rectangle(
        x + pnCaptionWindowScreenDesignAreaView.Left,
        y + pnCaptionWindowScreenDesignAreaView.Top,
        x + pnCaptionWindowScreenDesignAreaView.Left + pnCaptionWindowScreenDesignAreaView.Width,
        y + pnCaptionWindowScreenDesignAreaView.Top + pnCaptionWindowScreenDesignAreaView.Height);


      // Desenha a borda do form
      ARect := Rect(x, y,
        x + pnWindowScreenDesignAreaView.Width,
        y + pnWindowScreenDesignAreaView.Height);

      Canvas.Frame3d(ARect,
        pnWindowScreenDesignAreaView.BevelWidth,
        pnWindowScreenDesignAreaView.BevelOuter);

      InflateRect(ARect,
        -pnWindowScreenDesignAreaView.BorderWidth,
        -pnWindowScreenDesignAreaView.BorderWidth);

      Canvas.Frame3d(ARect,
        pnWindowScreenDesignAreaView.BevelWidth,
        pnWindowScreenDesignAreaView.BevelInner);

      // Desenha a 'mira'
      Canvas.Pen.Color := clGreen;
      Canvas.Frame(
        Width div 4,
        Height div 4,
        Width - (Width div 4),
        Height - (Height div 4));

      Canvas.Pen.Color := clRed;
      Canvas.Frame(0, 0, Width, Height);
      Canvas.Line(0, Height div 2, Width, Height div 2);
      Canvas.Line(Width div 2, 0, Width div 2, Height);

    end;

  end;

end;

procedure TKZLazDesktopInterface.edFindComponentButtonClick(Sender: TObject);
begin
  edFindComponent.Clear;
end;

procedure TKZLazDesktopInterface.edFindComponentChange(Sender: TObject);
var
  iP, iC, iC2: integer;
  _page: TTabSheet;
begin
  if TabComponentSearch = nil then begin
    TabComponentSearch := TTabComponentSearch.Create(Self);
    TabComponentSearch.Parent := pnComponents;
  end;

  with TabComponentSearch do begin
    DestroyButtons;
    Visible := False;
  end;

  if edFindComponent.Text = '' then begin
    TPageControl(pnComponents.FindChildControl('ComponentPageControl')).ActivePageIndex := 0;
    Exit;
  end;

  for iP := 0 to ComponentReg.IDEComponentPalette.Comps.Count - 1 do begin
    with ComponentReg.IDEComponentPalette do begin
        if Pos(LowerCase(edFindComponent.Text),
          LowerCase(Comps[iP].ComponentClass.ClassName )) <> 0
        then begin
          Self.TabComponentSearch.NewButton.Prepare(Comps[iP]);
        end;
    end;
  end;

  TabComponentSearch.Visible := True;
end;

procedure TKZLazDesktopInterface.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
	if KZConfigManager.ObjectInspector_Align <> KZCONF_ALIGN_SEPARATE then begin
		try
			SaveCFG('Inspectors', 'Width', IntToStr( pnlObjectInspector.Width ));
		except on e: Exception do ;
		end;
	end;

	if IDEDockMaster = Nil then begin
		SaveCFG('FormEditor', 'Width', IntToStr( Self.Width ));
		SaveCFG('FormEditor', 'Height', IntToStr( Self.Height ));
		SaveCFG('FormEditor', 'Top', IntToStr( Self.Top ));
		SaveCFG('FormEditor', 'Left', IntToStr( Self.Left ));
	end;
end;

procedure TKZLazDesktopInterface.imgWindowScreenPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  SetCapture(pnWindowScreenPreview.Handle);
  FPreviewMoving := True;
  FPreviewMovingPoint.X := x;
  FPreviewMovingPoint.Y := Y;
end;

procedure TKZLazDesktopInterface.imgWindowScreenPreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  if FPreviewMoving then begin
    pnWindowScreenPreview.Left :=
      pnWindowScreenPreview.Left - (FPreviewMovingPoint.x - X);
    pnWindowScreenPreview.Top :=
      pnWindowScreenPreview.Top - (FPreviewMovingPoint.Y - Y);
  end;
end;

procedure TKZLazDesktopInterface.imgWindowScreenPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if FPreviewMoving then begin
    ReleaseCapture;
    FPreviewMoving := False;

    with pnWindowScreenPreview do begin
      Left := max(Left - (FPreviewMovingPoint.X - X), 0);
      Top := max(Top - (FPreviewMovingPoint.Y - Y), 0);

      if FDesignPanel.LoadedFormIsValid then begin
        FDesignPanel.LoadedForm.SetBounds(
          round((Left / Parent.Width) * Screen.Width),
          round((Top / Parent.Height) * Screen.Height),
          FDesignPanel.LoadedForm.Width,
          FDesignPanel.LoadedForm.Height);

      end;

    end;
  end;

end;

procedure TKZLazDesktopInterface.pnDesignAreaViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  SetCapture(pnDesignAreaView.Handle);

  FDesignAreaViewMoving := True;
  FDesignAreaViewMovingPoint.X := x;
  FDesignAreaViewMovingPoint.Y := Y;

end;

procedure TKZLazDesktopInterface.pnDesignAreaViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin

  if FDesignAreaViewMoving then begin

    with pnDesignAreaView do begin
      Left := Left - (FDesignAreaViewMovingPoint.x - X);
      Top := Top - (FDesignAreaViewMovingPoint.Y - Y);
    end;

  end;

  imgDesignAreaView.Repaint;
end;

procedure TKZLazDesktopInterface.pnDesignAreaViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  if FDesignAreaViewMoving then begin
    ReleaseCapture;
    FDesignAreaViewMoving := False;

    with pnDesignAreaView do begin
      Left := max(Left - (FDesignAreaViewMovingPoint.X - X), 0);
      Top := max(Top - (FDesignAreaViewMovingPoint.Y - Y), 0);

      if FDesignPanel.LoadedFormIsValid then begin
        if KZ_REMOVE_FORM_BORDER or (not FDesignPanel.FFakeWindow.Visible) then begin
          FDesignPanel.SetBounds(
          (-FDesignPanel.LoadedForm.Left) - round(Left * (Screen.Width / Parent.Width)),
          (-FDesignPanel.LoadedForm.Top) - round(Top * (Screen.Height / Parent.Height)),
          FDesignPanel.Width,
          FDesignPanel.Height);
        end else begin
          FDesignPanel.SetBounds(
              (-FDesignPanel.FFakeWindow.Left) - round(Left * (Screen.Width / Parent.Width)) + 5,
              (-FDesignPanel.FFakeWindow.Top) - round(Top * (Screen.Height / Parent.Height)) + 5,
              FDesignPanel.Width,
              FDesignPanel.Height);
        end;

      end;
    end;
  end;
end;

procedure TKZLazDesktopInterface.SpeedButton1Click(Sender: TObject);
begin
  if FDesignPanel.LoadedFormIsValid then
    with FDesignPanel.LoadedForm do
    begin
      if Sender = SpeedButton1 then Height := Height - 10;
      if Sender = SpeedButton2 then Height := Height + 10;
      if Sender = SpeedButton3 then Width := Width - 10;
      if Sender = SpeedButton4 then Width := Width + 10;
    end;
end;

function TKZLazDesktopInterface.MainIDEForm: TCustomForm;
begin
  Result := Screen.FindForm('MainIDE');
end;

function TKZLazDesktopInterface.ObjectInspectorDlg: TObjectInspectorDlg;
begin
  Result := TObjectInspectorDlg(IDEWindowCreators.GetForm(
    DefaultObjectInspectorName, True));
end;

function TKZLazDesktopInterface.pnlSpeedButtons: TPanel;
begin
  Result := TPanel(LazarusIDE.OwningComponent.FindComponent('pnlSpeedButtons'));
end;

function TKZLazDesktopInterface.pnlRightSpeedButtons: TPanel;
begin
  Result := TPanel(LazarusIDE.OwningComponent.FindComponent('pnlRightSpeedButtons'));
end;

function TKZLazDesktopInterface.ComponentPageControl: TPageControl;
begin
  Result := TPageControl(LazarusIDE.OwningComponent.FindComponent('ComponentPageControl'));
  if Result = nil then begin
    Result:= TPageControl(pnComponents.FindChildControl('ComponentPageControl'));
  end;
end;

procedure TKZLazDesktopInterface.SetupScreen;
begin
	if KZConfigManager.ObjectInspector_Align <> KZCONF_ALIGN_SEPARATE then
		Icon := ObjectInspectorDlg.Icon;
	if IDEDockMaster <> nil then begin
		IDEDockMaster.ShowForm(Self,False);
		FDesignPanel.LoadLastForm;
	end;
	Visible := True;

  //Screen.AddHandlerFormAdded(@Screen_FormAdded, False);
  //Screen.AddHandlerActiveFormChanged(@Screen_ActiveFormChanged, False);
  //Screen.AddHandlerActiveControlChanged(@Screen_ActiveControlChanged, False);
end;

procedure TKZLazDesktopInterface.MainIDEFormShow(TheOwner: TObject);
begin
  SetupDefaultBars;
end;

procedure TKZLazDesktopInterface.OnIDERestoreWindows(Sender: TObject);
begin
  UpdateFromOptions;

  if FDesignPanel <> Nil then
    FDesignPanel.UpdateScreenForms;

  //bug in docking solutions and object inspector
  if (IDEDockMaster <> nil) and (not ObjectInspectorDlg.Visible) then begin
	ObjInspector_OldShow := ObjectInspectorDlg.OnShow;
	ObjectInspectorDlg.OnShow := @ObjInspector_OnShow;
  end;
end;

procedure TKZLazDesktopInterface.ObjInspector_OnShow(Sender: TObject);
begin
	if Sender = ObjectInspectorDlg then begin
		if ObjInspector_OldShow <> Nil then ObjInspector_OldShow(Sender);
		if (ObjectInspectorDlg.Parent <> Nil) and (ObjectInspectorDlg.Parent.ClassType.InheritsFrom(TCustomForm)) then begin
			ObjectInspectorDlg.OnShow := ObjInspector_OldShow;
			ObjInspector_OldShow := TCustomForm(ObjectInspectorDlg.Parent).OnShow;
            TCustomForm(ObjectInspectorDlg.Parent).OnShow := @ObjInspector_OnShow;
			ObjInspector_OldHide := TCustomForm(ObjectInspectorDlg.Parent).OnHide;
			TCustomForm(ObjectInspectorDlg.Parent).OnHide := @ObjInspector_OnHide;
		end;
	end else if Sender = ObjectInspectorDlg.Parent then begin
		if ObjInspector_OldShow <> Nil then ObjInspector_OldShow(Sender);
   		TEditButton(ObjectInspectorDlg.FindComponent('CompFilterEdit')).Enabled := True;
	end;
end;

procedure TKZLazDesktopInterface.ObjInspector_OnHide(Sender: TObject);
begin
    if ObjInspector_OldHide <> Nil then ObjInspector_OldHide(Sender);
	TEditButton(ObjectInspectorDlg.FindComponent('CompFilterEdit')).Enabled := False;
end;

function TKZLazDesktopInterface.OnProjectClose(Sender: TObject;
  AProject: TLazProject): TModalResult;
begin

end;

procedure TKZLazDesktopInterface.DesignPanel_OnLoadForm(Sender: TObject);
begin

end;

procedure TKZLazDesktopInterface.DesignPanel_OnLoadedFormChangeBounds(
  Sender: TObject);
begin
  if FDesignPanel.LoadedFormIsValid then begin
    SetupPreviewFormDesign(FDesignPanel.LoadedForm);
    SetupFormDesignAreaView(FDesignPanel.LoadedForm);
  end;
end;

function TKZLazDesktopInterface.IsFormDesign(fm: TCustomForm): boolean;
begin
  Result := IsFormDesign_Internal(fm);
end;

function TKZLazDesktopInterface.IsObjectInspector(fm: TCustomForm): boolean;
begin
  Result := (fm = ObjectInspectorDlg) and (not (csDesignInstance in fm.ComponentState));
end;

function TKZLazDesktopInterface.PixelPerc(vl, perc: single): integer;
begin
  Result := Round((vl / 100) * perc);
end;

procedure TKZLazDesktopInterface.SetupPreviewFormDesign(_Form: TCustomForm);
begin
  if (FPreviewMoving) then
    Exit;

  with pnWindowScreenPreview do begin
    Width := PixelPerc(_Form.Width, PreviewFactor);
    Height := PixelPerc(_Form.Height, PreviewFactor);
    Top := PixelPerc(_Form.Top, PreviewFactor);
    Left := PixelPerc(_Form.Left, PreviewFactor);
    DoubleBuffered := True;
  end;

  with pnCaptionWindowScreenPreview do begin
    Height := PixelPerc(Parent.Height, 15);
  end;
end;

procedure TKZLazDesktopInterface.SetupFormDesignAreaView(_Form: TCustomForm);
begin
  with pnWindowScreenDesignAreaView do begin
    Width := PixelPerc(_Form.Width, PreviewFactor);
    Height := PixelPerc(_Form.Height, PreviewFactor);
    Top := 3;
    Left := 3;
    DoubleBuffered := True;
  end;

  with pnCaptionWindowScreenDesignAreaView do begin
    Height := PixelPerc(Parent.Height, 15);
  end;

  with pnDesignAreaView do begin
    Left := 0;
    Top := 0;
    Width  := PixelPerc(pnDesign.Width, PreviewFactor);
    Height := PixelPerc(pnDesign.Height, PreviewFactor);
  end;

end;

procedure TKZLazDesktopInterface.SetupDefaultBars;
var
  i, j, k: integer;
  ccomp: TComponent;
  tpc: TPageControl;
begin
  with TKZLazDesktopInterface(Self) do begin
    if (pnlRightSpeedButtons <> nil) then with pnlRightSpeedButtons do begin
      Parent := pnComponents;
    end;

    if ComponentPageControl <> nil then
    begin
      {$IFDEF MSWINDOWS}
      pnComponents.Height := GetSystemMetrics(SM_CYCAPTION) + 35;
      {$ELSE}
      pnComponents.Height := 70;
      {$ENDIF}

      ComponentPageControl.Parent := pnComponents;
    end;
    for i := 0 to LazarusIDE.OwningComponent.ComponentCount-1 do begin
        ccomp := LazarusIDE.OwningComponent.Components[i];
        if ccomp.ClassName = 'TSplitter' then begin
          TSplitter(ccomp).Parent:=nil;
        end;
        //if ccomp.ClassName = 'TCoolBar' then begin
          //TCoolBar(ccomp).Align:=alTop;
          //TCoolBar(ccomp).Height:=30;
        //end;
		end;
  end;
end;

procedure TKZLazDesktopInterface.ActivateDesigner(Sender: TObject;
	AEditor: TSourceEditorInterface; AComponentPaletteClassSelected: Boolean);
var
	aRect: TRect;
begin
	if not Visible then Visible := True;
    if not Enabled then Enabled := True;
	pnComponents.Visible := ComponentPageControl.Visible;
  	if IDEDockMaster <> nil then begin
		IDEDockMaster.ShowForm(Self,True);
  	end else begin
		BringToFront;
	end;
end;

procedure TKZLazDesktopInterface.Execute;
begin
  Self.Left := 0;
  Self.Top := 0;
  Self.Visible := False;

  //MainIDEForm.AddHandlerOnResize(@MainIDEFormResize);
  MainIDEForm.AddHandlerFirstShow(@MainIDEFormShow, False);

  ObjectInspectorManager := TKZObjectInspectorManager.Create(pnlObjectInspector, KZ_OBJECT_INSPECTOR_PARENT_TYPE);

  LazarusIDE.AddHandlerOnIDERestoreWindows(@OnIDERestoreWindows);
  LazarusIDE.AddHandlerOnProjectClose(@OnProjectClose);
  LazarusIDE.AddHandlerOnShowDesignerFormOfSource(@ActivateDesigner);

  SetupScreen;
end;

procedure TKZLazDesktopInterface.UpdateFromOptions;
begin
	if KZConfigManager.ObjectInspector_Align <> KZCONF_ALIGN_SEPARATE then begin
		pnlObjectInspector.Visible := True;
		spObjectInspector.Visible := True;
		ObjectInspectorManager.Prepare(ObjectInspectorDlg);

		if KZConfigManager.ObjectInspector_Align = KZCONF_ALIGN_LEFT then begin
		  pnlObjectInspector.Align := alLeft;
		  spObjectInspector.Align := alLeft;
		  spObjectInspector.Left := pnlObjectInspector.Left + pnlObjectInspector.Width + 2;
		end else begin
		  pnlObjectInspector.Align := alRight;
		  spObjectInspector.Align := alRight;
		  spObjectInspector.Left := pnlObjectInspector.Left - pnlObjectInspector.Width - 2;
		end;
	end else begin
		ObjectInspectorManager.RemoveForm(ObjectInspectorDlg);
		pnlObjectInspector.Visible := False;
		spObjectInspector.Visible := False;
	end;
	if KZConfigManager.ComponentFind_Align = KZCONF_ALIGN_LEFT then begin
		pnFindComponent.Visible := True;
		pnFindComponent.Align := alLeft;
	end else if KZConfigManager.ComponentFind_Align = KZCONF_ALIGN_RIGHT then begin;
		pnFindComponent.Visible := True;
		pnFindComponent.Align := alRight;
	end else
		pnFindComponent.Visible := False;

	pnDesign.Color := StringToColor(KZConfigManager.DesignEditor_BkCollor);
end;

end.

