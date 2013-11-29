// Productivity Experts by Chua Chee Wee, Singapore
//
// Chua Chee Wee,
// Singapore
// 19 April 2005.
//
// 1. Region Marking Expert
// 2. Code Comments Expert
// 3. XML Doc comments Expert
// 4. Auto Extend Comments Expert
//
// Blog: http://chuacw.ath.cx/chuacw/category/78.aspx

unit ProductivityExpertImpl;

interface

uses
  SysUtils, Classes, ToolsAPI;

type
  TProductivityExpertBinding = class(TNotifierObject, IOTAKeyboardBinding)
  protected
    Event: TObject;
  public


    procedure CodeComments(const Context: IOTAKeyContext;
      KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
    procedure MarkSelectionAsRegion(const Context: IOTAKeyContext;
      KeyCode: TShortCut; var BindingResult: TKeyBindingResult);


    procedure XMLComments(const Context: IOTAKeyContext;
      KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
    procedure AutoExtendComments(const Context: IOTAKeyContext;
      KeyCode: TShortCut; var BindingResult: TKeyBindingResult);

    procedure NullCmd(const Context: IOTAKeyContext;
      KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
    procedure DefKeyProc(const Context: IOTAKeyContext;
      KeyCode: TShortCut; var BindingResult: TKeyBindingResult);

    // IOTAKeyBoardBinding
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses
  Clipbrd, Dialogs, Menus, Windows, XMLCommentsParserImpl,
{$IF CompilerVersion>=16.0}
  RegionExpertImpl,
{$IFEND}
  OTAUtils, 
  SyncObjs, CSharpParserImpl, DelphiParserImpl;

procedure Register;
var
  KS: IOTAKeyboardServices;
  BR: TKeyBindingRec;
begin
  KS := BorlandIDEServices as IOTAKeyBoardServices;
  (BorlandIDEServices as IOTAKeyboardDiagnostics).KeyTracing := False;
{$IF CompilerVersion >= 16.0}
  KS.AddKeyboardBinding(TRegionExpertBinding.Create);
{$IFEND}
  KS.AddKeyBoardBinding(TProductivityExpertBinding.Create);

end;

function TProductivityExpertBinding.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TProductivityExpertBinding.GetDisplayName;
begin
// The way it should appear in the IDE's Editor Options, Key Mappings
  Result := 'Chee Wee''s Productivity Experts';
end;

function TProductivityExpertBinding.GetName;
begin
  Result := 'chuacw.Productivity Experts'; // must be unique!!!
end;

procedure TProductivityExpertBinding.MarkSelectionAsRegion(const Context: IOTAKeyContext; KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
var
  LEditBuffer: IOTAEditBuffer;
  ep: IOTAEditPosition;
  eb: IOTAEditBlock;
  IndentText,
    Region, ClipboardData, EOL, Data, RegionPrompt: string;
begin
  Region := 'REGION1';
{$IF CompilerVersion >= 16.0}
  RegionPrompt := Format('Please enter the name of the new %s region',
    [PersonalityToDisplayName(ProjectPersonality(Context.EditBuffer.Module.Owners[0]))]);
{$ELSE}
  RegionPrompt := 'Please enter the name of the new region';
{$IFEND}
  if InputQuery('Chee Wee''s Productivity Experts', RegionPrompt, Region) then
    begin

      IndentText := '';
      LEditBuffer := Context.EditBuffer;
      ep := LEditBuffer.EditPosition;
      eb := LEditBuffer.EditBlock;
      ep.Save;
      try
        eb.Copy(False);
        Data := Clipboard.AsText;

// Detect the number of indentation and fill in the spaces appropriately...
        if LEditBuffer.BufferOptions.AutoIndent then
          IndentText := GetIndentation(Data);

        if (Length(Data) > 0) and
          ((Data[Length(Data) - 1] = #13) and (Data[Length(Data)] = #10)) then
          EOL := '' else
          EOL := #13#10;
        ClipboardData := Data;

{$IF CompilerVersion >= 16.0}
        case ProjectPersonality(LEditBuffer.Module.Owners[0]) of
          DelphiWin32, DelphiNET:
            Data := Format('%s{$REGION ''%s''}'#13#10'%s%s%0:s{$ENDREGION ''%1:s''}'#13#10,
              [IndentText, Region, ClipboardData, EOL]);
          CSharp:
            Data := Format('%s#region %s'#13#10'%s%s%0:s#endregion %1:s'#13#10,
              [IndentText, Region, ClipboardData, EOL]);
        end;
{$ELSE}
        Data := Format('%s{$REGION ''%s''}'#13#10'%s%s%0:s{$ENDREGION ''%1:s''}'#13#10,
          [IndentText, Region, ClipboardData, EOL]);
{$IFEND}

        Clipboard.AsText := Data;
        ep.Paste;
      finally
        ep.Restore;
      end;
    end;
  BindingResult := krHandled;
end;

// Specify the key binding that performs the action

procedure TProductivityExpertBinding.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
const
  kfAll = kfImplicitKeypad or kfImplicitModifier or kfImplicitShift;
  kfDefault = kfAll;
var
  Slash1, Slash2: Word;
  MS: IOTAMessageServices;
  MG: IOTAMessageGroup;
  LEvent: TEvent;
begin
{$IFDEF DEBUG}
  MS := BorlandIDEServices as IOTAMessageServices;
  MG := MS.AddMessageGroup(GetDisplayName);
  MS.ClearMessageGroup(MG);
{$IF CompilerVersion>=16.0}
  MG.AutoScroll := True;
{$IFEND}
  MS.AddTitleMessage('Loading Chee Wee''s Productivity Experts...', MG);
{$ENDIF}

  // TextToShortCut('/') may not return the correct value on certain systems
  // The correct value is $BF, which is VK_OEM_2 which varies on different keyboards
  // see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/WinUI/WindowsUserInterface/UserInput/VirtualKeyCodes.asp
  Slash1 := VkKeyScan('/');
  Slash2 := TextToShortCut('/');

  BindingServices.AddKeyBinding([Shortcut(Ord('D'), [ssCtrl])],
    MarkSelectionAsRegion, nil, kfDefault);

  // Somehow, Ctrl+/ doesn't work since D7, so, use Ctrl K+/
  // See Erik Berry's excellent site,
  // "Known bugs in the Delphi 7 Open Tools API (some apply to Delphi/BCB 6)" at
  // http://www.gexperts.org/opentools/
  BindingServices.AddKeyBinding([Shortcut(Ord('K'), [ssCtrl]), Slash1],
    CodeComments, nil, kfDefault);
  if Slash1 <> Slash2 then
    BindingServices.AddKeyBinding([Shortcut(Ord('K'), [ssCtrl]), Slash2],
      CodeComments, nil, kfDefault);

  // There are two characters on the keyboard that can match / so
  // hook all possible keys to the XMLComments method
  BindingServices.AddKeyBinding([Slash1], XMLComments, nil, 0);
  if Slash1 <> Slash2 then
    BindingServices.AddKeyBinding([Slash2], XMLComments, nil, 0);
  if (VK_DIVIDE <> Slash1) and (VK_DIVIDE <> Slash2) then
    BindingServices.AddKeyBinding([VK_DIVIDE], XMLComments, nil, 0);

  BindingServices.AddKeyBinding([VK_RETURN], AutoExtendComments, nil);
  
  LEvent := nil;
{$IFDEF DEBUG}
  LEvent := TEvent.Create(nil, True, False, 'Delphi2005Debug');
  if LEvent.WaitFor(500) = wrSignaled then
    DebugBreak else
    LEvent.SetEvent;
  Event := LEvent;

  MS.AddTitleMessage('Chee Wee''s Productivity Experts loaded.', MG);
{$ENDIF}
end;

procedure TProductivityExpertBinding.NullCmd(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  BindingResult := krHandled;
end;

procedure TProductivityExpertBinding.XMLComments(const Context: IOTAKeyContext;
  KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
var
  XML, Text, Language: string;
  IndentCount, I, Row, Col, Key,
    NewRow, NewCol: Word;
  Shift: TShiftState;
  ep: IOTAEditPosition;
  eb: IOTAEditBlock;
  InsertChar: Boolean;
  MS: IOTAMessageServices;
  MG: IOTAMessageGroup;
  SL: TStringList;
  LAutoIndent: Boolean;
{$IFDEF DEBUG}
  {$IF CompilerVersion>=16.0}
    LThisProject, LCurrentProject: IOTAProject;
  {$IFEND}
{$ENDIF}
begin
{$IFDEF DEBUG}
  MS := BorlandIDEServices as IOTAMessageServices;
  MG := MS.AddMessageGroup(GetDisplayName);
{$IF CompilerVersion>=16.0}
  MG.AutoScroll := True;
{$IFEND}
{$ENDIF}
  ShortCutToKey(KeyCode, Key, Shift);

{$IFDEF DEBUG}
{$IF CompilerVersion >= 16.0}
// Support for multiple personalities
  LCurrentProject := GetCurrentProject; LThisProject := nil;
  for I := 0 to Context.EditBuffer.Module.OwnerCount - 1 do
    begin
      LThisProject := Context.EditBuffer.Module.Owners[I];
      MS.AddTitleMessage(Format('Project personality: %s Filename: %s', [
        LThisProject.Personality, LThisProject.FileName]), MG);
    end;
  if Assigned(LCurrentProject) then
    MS.AddTitleMessage(Format('Curr project personality: %s', [LCurrentProject.Personality]), MG);
{$IFEND}
{$ENDIF}
  ep := Context.EditBuffer.EditPosition;
  Row := ep.Row;
  Col := ep.Column;
  eb := Context.EditBuffer.EditBlock;
  ep.Save;
  try
    Text := ep.RipText('/', rfBackward);
    InsertChar := Text <> '//';
{$IFDEF DEBUG}
    OutputDebugString(PChar(Text));
{$ENDIF}
  finally
    ep.Restore;
  end;
  if InsertChar then
    begin
      ep.InsertCharacter(Char(Key));
    end else
    begin /// XMLComment mode!!!
      Text := ep.Read(1024);
      try
        Language := Context.EditBuffer.EditOptions.IDString;
        if Language = cDefEdPascal then
          XML := ParseDelphiDeclaration(Text) else
        if Language = cDefEdCSharp then
          XML := ParseCSharpDeclaration(Text);
{$IFDEF DEBUG}
        OutputDebugString(PChar(Text));
{$ENDIF}
        ep.Move(Row, Col - 2);
        eb.BeginBlock;
        ep.Move(Row, Col);
        eb.EndBlock;
        eb.Cut(False);

// Take care of auto-indent here
        LAutoIndent := Context.EditBuffer.BufferOptions.AutoIndent;
        IndentCount := Col - 3;
        if LAutoIndent then
          begin
            SL := TStringList.Create;
            SL.Text := XML;
            for I := 1 to SL.Count - 1 do
              SL[I] := StringOfChar(' ', IndentCount) + SL[I];
            XML := SL.Text;
            SL.Free;
          end;

// Remove extra linefeed
        if (Text[1] = #13) and (Text[2] = #10) then
          if (XML[Length(XML) - 1] = #13) and (XML[Length(XML)] = #10) then
            Delete(XML, Length(XML) - 1, 2);

        Clipboard.AsText := XML;
        ep.Paste;

// Indent the original text
        if LAutoIndent then
          ep.InsertText(StringOfChar(' ', IndentCount));

// Move the cursor to the start of the new XML Doc, ready for entering
// text
        NewRow := Row + 1;
        NewCol := 5;
        if LAutoIndent then
          Inc(NewCol, IndentCount);
        ep.Move(NewRow, NewCol);

      except
        on E: Exception do
          begin
{$IFDEF DEBUG}
            MS.AddTitleMessage(E.Message, MG);
            MS.AddTitleMessage('Info: Inserting character pressed as last resort', MG);
{$ENDIF}
            ep.InsertCharacter(Char(Key));
          end;
      end;
    end;
  BindingResult := krHandled;
end;

/// <summary>
///
/// </summary>
/// <param name="Context" type="IOTAKeyContext"></param>
/// <param name="KeyCode" type="TShortCut"></param>
/// <param name="BindingResult" type="TKeyBindingResult"></param>
procedure TProductivityExpertBinding.CodeComments(const Context: IOTAKeyContext;
  KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
var
  I: Integer;
  ep: IOTAEditPosition;
  eb: IOTAEditBlock;
  Data: string;
  SL: TStringList;
begin
  BindingResult := krHandled;
  SL := nil;
  ep := Context.EditBuffer.EditPosition;
  eb := Context.EditBuffer.EditBlock;
  ep.Save;
  try
    if eb.Size = 0 then
      begin
        ep.MoveBOL;
        ep.InsertText('// ');
        exit;
      end;
    eb.Copy(False);
    Data := Clipboard.AsText;
    SL := TStringList.Create;
    SL.Text := Data;
    for I := 0 to SL.Count - 1 do
      SL[I] := '// ' + SL[I];
    Data := SL.Text;
    Clipboard.AsText := Data;
    ep.Paste;
  finally
    ep.Restore;
    SL.Free;
  end;
end;

{.$REGION 'AutoExtendComments'}
/// <summary>
/// Allows comments to be automatically extended, if the enter key is
/// pressed at the end of a comment line, and the next non-empty line is
/// also a comment, beginning with // or ///
/// </summary>
/// <param name="Context" type="IOTAKeyContext"></param>
/// <param name="KeyCode" type="TShortCut"></param>
/// <param name="BindingResult" type="TKeyBindingResult"></param>

procedure TProductivityExpertBinding.AutoExtendComments(const Context: IOTAKeyContext;
  KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
var
  Key, Row, Column: Word;
  Shift: TShiftState;
  ep: IOTAEditPosition;
  Slashes, Text: string;
  MS: IOTAMessageServices;
  MG: IOTAMessageGroup;
  CIS: IOTACodeInsightServices;
  CIM: IOTACodeInsightManager;
begin
  BindingResult := krUnhandled;
// Do not interfere with CodeInsight, so if it's activated
// quit immediately...
  CIS := BorlandIDEServices as IOTACodeInsightServices;
  CIS.GetCurrentCodeInsightManager(CIM);
  if Assigned(CIM) then
    begin
      BindingResult := krUnhandled;
      exit;
    end;

  ShortCutToKey(KeyCode, Key, Shift);

  try
    ep := Context.EditBuffer.EditPosition;
    ep.InsertCharacter(Char(Key));
    BindingResult := krHandled;
    Row := ep.Row; Column := ep.Column;
    ep.Save;
    if Column <> 1 then ep.Move(Row, 1);
    Text := ep.Read(1024);
    ep.Restore;
    Slashes := '//';
    if (Shift = [ssShift]) or WithinCodeCommentsRegion(Text, Slashes) then
      ep.InsertText(Format('%s ', [Slashes]));
  except
    on E: Exception do
      begin
        MS := BorlandIDEServices as IOTAMessageServices;
        MG := MS.AddMessageGroup(GetDisplayName);
{$IF CompilerVersion>=16.0}
        MG.AutoScroll := True;
{$IFEND}
        MS.AddTitleMessage(E.Message, MG);
      end;
  end;

end;
{.$ENDREGION 'AutoExtendComments'}

procedure TProductivityExpertBinding.DefKeyProc(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
  BindingResult := krNextProc;
end;

destructor TProductivityExpertBinding.Destroy;
begin
  Event.Free;
  inherited;
end;

end.

