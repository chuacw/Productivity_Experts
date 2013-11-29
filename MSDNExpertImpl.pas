unit MSDNExpertImpl;

interface

procedure Register;

implementation
uses
  ToolsAPI, Windows, Menus, Classes, ShellAPI, SysUtils, WelcomePageIntf;

type
  TMSDNHelpExpertBinding = class(TNotifierObject, IOTAKeyboardBinding)
  protected
    FMSDNQuery: string;
  public

    constructor Create(const AMSDNQuery: string = '');

    procedure MSDNHelp(const Context: IOTAKeyContext;
      KeyCode: TShortCut; var BindingResult: TKeyBindingResult);

    // IOTAKeyBoardBinding
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  end;

procedure Register;
var
  KS: IOTAKeyboardServices;
begin
  KS := BorlandIDEServices as IOTAKeyBoardServices;
  KS.AddKeyboardBinding(TMSDNHelpExpertBinding.Create);
end;

{ TMSDNHelpExpertBinding }

procedure TMSDNHelpExpertBinding.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([Shortcut(VK_F1, [ssCtrl])], MSDNHelp, nil);
end;

constructor TMSDNHelpExpertBinding.Create(const AMSDNQuery: string = '');
begin
  inherited Create;
  if AMSDNQuery <> '' then
    FMSDNQuery := AMSDNQuery else
    FMSDNQuery := 'http://search.microsoft.com/search/results.aspx?view=msdn&qu=%s';
end;

function TMSDNHelpExpertBinding.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TMSDNHelpExpertBinding.GetDisplayName: string;
begin
  Result := 'Chee Wee''s MSDN Expert';
end;

function TMSDNHelpExpertBinding.GetName: string;
begin
  Result := 'chuacw.ProductivityExperts.MSDNHelp';
end;

type
  THelpThread = class(TThread)
  protected
    FSearchString: string;
    procedure Execute; override;
  public
    constructor Create(const ASearchString: string);
  end;

constructor THelpThread.Create(const ASearchString: string);
begin
  inherited Create(True);
  FSearchString := ASearchString;
  FreeOnTerminate := True;
  Resume;
end;

procedure THelpThread.Execute;
begin
  ShellExecute(0, 'open', PChar(FSearchString), nil, nil, SW_SHOW);
end;

procedure TMSDNHelpExpertBinding.MSDNHelp(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var
  ep: IOTAEditPosition;
  EditView: IOTAEditView;
  CursorPos: TOTAEditPos;
  StartCol, EndCol, WordLen, Element, LineFlag: Integer;
  Identifier, LQueryString: string;
begin
  EditView := Context.EditBuffer.TopView;
  ep := EditView.Position;
  CursorPos := EditView.CursorPos;
  EditView.GetAttributeAtPos(CursorPos, True, Element, LineFlag);
  if Element = atWhiteSpace then
    begin
      Dec(CursorPos.Col);
      EditView.GetAttributeAtPos(CursorPos, True, Element, LineFlag);
      if Element = atWhiteSpace then
        begin
          Inc(CursorPos.Col, 2);
          EditView.GetAttributeAtPos(CursorPos, True, Element, LineFlag);
        end;
    end;
  if Element in [atIdentifier, atReservedWord] then
    begin
// The cursor can be anywhere on the word, or at the end of a word
// If cursor is at the middle of a word, then extract the word
      if ep.IsWordCharacter then
        begin
          ep.Save;
          try
            EndCol := CursorPos.Col;
            while (ep.IsWordCharacter) and (ep.Column > 1) do
              ep.MoveRelative(0, -1);
            StartCol := ep.Column + 1;
            WordLen := EndCol - StartCol;
            ep.Move(ep.Row, EndCol);
            while ep.IsWordCharacter do
              begin
                ep.MoveRelative(0, 1);
                Inc(WordLen);
              end;
            ep.MoveRelative(0, -WordLen);
            Identifier := ep.Read(WordLen);
          finally
            ep.Restore;
          end;
        end else
        begin
// If cursor is at the end of a word, then extract it
          Identifier := ep.RipText('_', rfBackward or rfIncludeAlphaChars or rfIncludeNumericChars);
        end;
      LQueryString := Format(FMSDNQuery, [Identifier]);
// Run search in background so as not to suspend IDE
      if not GoURL(LQueryString) then
        THelpThread.Create(LQueryString);
    end;
end;

end.

