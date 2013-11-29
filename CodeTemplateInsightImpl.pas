unit CodeTemplateInsightImpl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolsAPI, CodeTemplatesImpl, StdCtrls;

type
  TCodeTemplateInsight = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    FEditPosition: IOTAEditPosition;
    FCodeTemplate: TCodeTemplate;
    FEditView: IOTAEditView;
    FIdentifierLength: Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure InsertTemplate;
  public
    { Public declarations }
    constructor Create(const AEditPosition: IOTAEditPosition;
      const ACodeTemplate: TCodeTemplate; const AEditView: IOTAEditView;
      const AIdentifierLength: Integer); reintroduce;
  end;

var
  CodeTemplateInsight: TCodeTemplateInsight;

implementation
uses CommCtrl;
{$R *.dfm}

function GetClassName(AControl: TWinControl): string;
begin
  SetLength(Result, 1024);
  SetLength(Result, Windows.GetClassName(AControl.Handle, PChar(Result), Length(Result) - 1));
end;

const
  TTS_BALLOON = $40;
  TTM_SETTITLE = (WM_USER + 32);
  TTM_POP = (WM_USER + 28);
  TTM_POPUP = (WM_USER + 34);

procedure TCodeTemplateInsight.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP or TTS_BALLOON;
  Params.WinClassName := TOOLTIPS_CLASS;
end;

procedure TCodeTemplateInsight.FormClick(Sender: TObject);
begin
  InsertTemplate;
  Release;
end;

procedure TCodeTemplateInsight.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    Chr(VK_SPACE),
      Chr(VK_RETURN):
      begin
        InsertTemplate;
        Release;
      end;
  else
    if IsCharAlphaNumeric(Key) then
      begin
        FEditPosition.InsertCharacter(Key);
        FEditView.Paint;
      end;
    Release;
  end;
end;

procedure TCodeTemplateInsight.FormDeactivate(Sender: TObject);
begin
  Release;
end;

constructor TCodeTemplateInsight.Create(const AEditPosition: IOTAEditPosition;
  const ACodeTemplate: TCodeTemplate; const AEditView: IOTAEditView;
  const AIdentifierLength: Integer);
var
  LForm: TCustomForm;
  LComponent: TComponent;
  LControl: TWinControl;
  I: Integer;
  LClassName: string;
  LCursorPoint, GCursorPoint: TPoint;
  EditorWnd: THandle;
begin
  inherited Create(nil);
  FEditPosition := AEditPosition;
  FCodeTemplate := ACodeTemplate;
  FEditView := AEditView;

  EditorWnd := GetFocus;
  GetCaretPos(LCursorPoint);
  GCursorPoint := LCursorPoint;
  Windows.ClientToScreen(EditorWnd, GCursorPoint);
(*
  // The following code requires undocumented knowledge, the above does
  // not
  LForm := AEditView.GetEditWindow.Form;
  for I := 0 to LForm.ComponentCount - 1 do
    begin
      LComponent := LForm.Components[I];
      if LComponent is TWinControl then
        begin
          LControl := TWinControl(LComponent);
          if LControl.HandleAllocated then
            begin
              LClassName := GetClassName(LControl);
              if LClassName = 'TEditControl' then
                begin
                  GetCaretPos(LCursorPoint);
                  GCursorPoint := LControl.ClientToScreen(LCursorPoint);
                  Break;
                end;
            end;
        end;
    end;
*)
 // + 10 because we don't want to be too near the cursor actually
  Left := GCursorPoint.X + 10;
  Top := GCursorPoint.Y;

  FIdentifierLength := AIdentifierLength;
  Label2.Caption := ACodeTemplate.Template;
end;

procedure TCodeTemplateInsight.InsertTemplate;
var
  ep: IOTAEditPosition;
  Template, Template1, Template2: string;
  SL: TStringList;
  I, Column, PipeIndex: Integer;
  LView: IOTAEditView;
  CursorPos: TOTAEditPos;
begin
  ep := FEditPosition; LView := FEditView;
  ep.MoveRelative(0, -FIdentifierLength);
  ep.Delete(FIdentifierLength);
  Template := FCodeTemplate.Template;
  SL := TStringList.Create;
  try
    SL.Text := Template;
    if (SL.Count > 1) and (LView.Buffer.EditOptions.BufferOptions.AutoIndent) then
      begin
        Column := ep.Column;
        Template2 := #13#10;
        for I := 0 to SL.Count - 1 do
          begin
            ep.Move(ep.Row, Column);
            Template1 := SL[I];
            PipeIndex := Pos('|', Template1);
            if PipeIndex > 0 then
              begin
                CursorPos.Line := ep.Row;
                CursorPos.Col := PipeIndex + Column - 1;
                Delete(Template1, PipeIndex, 1);
              end;
            if I = SL.Count - 1 then
              Template2 := '';
            Template1 := Format('%s%s', [Template1, Template2]);
            ep.InsertText(Template1);
          end;
        ep.Move(CursorPos.Line, CursorPos.Col);
      end else
      begin
        I := Pos('|', Template);
        Template1 := Copy(Template, 1, I - 1);
        Template2 := Copy(Template, I + 1, Length(Template));
        ep.InsertText(Template1);
        CursorPos := LView.CursorPos;
        ep.InsertText(Template2);
        LView.CursorPos := CursorPos;
      end;
  finally
    SL.Free;
  end;
  LView.Paint;
end;

procedure TCodeTemplateInsight.FormDestroy(Sender: TObject);
begin
  CodeTemplateInsight := nil;
end;

end.

