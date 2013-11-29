unit CSharpParserImpl;

interface

function ParseCSharpDeclaration(const Declaration: string): string;

implementation
uses
  XMLCommentsParserImpl, DeclParserIntf, TextUtils, SysUtils;

type
  TCSharpRoutineDeclaration = class(TRoutineDeclaration)
  protected
    function IsFunction(const AText: string): Boolean; override;
  end;

{ TCSharpRoutineDeclaration }

function TCSharpRoutineDeclaration.IsFunction(const AText: string): Boolean;
begin
  Result := AText <> 'void';
end;


function ParseVarDeclaration(const AText: string): string;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  Submatches: ISubMatches;
  Visibility, TypeName, VarName: string;
begin
  Parser := CoRegExp.Create;
  Parser.Global := True;

  Parser.Pattern :=
  '(private|protected|public)?'+   // Visibility
  '\s'+                           // Space
  '((\w+\.)*'+                     // Namespace
    '\w+)'+                        // Type
  '\s'+                            // Space
  '(\w+)'+                         // Variable name
  '(,\w+)*'+                       // Optional, more variables
  '(=[^;]*)?'+                     // Default Value
  ';';

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  Match := MatchCollection.Item[0] as IMatch2;
  Submatches := Match.SubMatches as ISubMatches;
  Visibility := Submatches[0];
  TypeName := Submatches[1];
  VarName := Submatches[3];
end;

function ParseTypeName(const AText: string): string;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  Submatches: ISubMatches;
  Text: string;
begin
  Parser := CoRegExp.Create;
  Parser.Global := True;

  // Look for one of the following
  // 1) : Type
  // 2) : [Namespace.[Namespace. ...]]Type

  Parser.Pattern := '(in|out|ref)?\s*(((\w+\.)*\w+)(\[\])?)\s?(\w+)(=[^,]*)?(,)?';

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  Match := MatchCollection.Item[0] as IMatch2;
  Submatches := Match.SubMatches as ISubMatches;
  Text := Submatches[1];
  Result := Text;
end;

function ParseDefaultValue(const AText: string): string;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  Submatches: ISubMatches;
  Text: string;
begin
  Result := '';
  Parser := CoRegExp.Create;
  Parser.Global := True;

  // Look for one of
  // 1) = 'some string'
  // 2) = Identifier
  // 3) = nnnn (where nnnn is digits) // not supporting real numbers...
  Parser.Pattern := '\s*\=\s*(''.*''|\w+|(\+|\-)?\d+\.\d+)';

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;

  if MatchCollection.Count = 0 then exit;
  Match := MatchCollection.Item[0] as IMatch2;
  Submatches := Match.SubMatches as ISubMatches;
  Text := Submatches[0];
  Result := Text;
end;

procedure ParseParamNames(Routine: TRoutineDeclaration; const AText: string);
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  Submatches: ISubMatches;
  I: Integer;
  ParamName, Token, TypeName, Default: string;
begin
  TypeName := ParseTypeName(AText);
  Default := '';
  // Default := ParseDefaultValue(AText);
  Parser := CoRegExp.Create;
  Parser.IgnoreCase := True;

// Look for identifiers, or identifiers: 
  Parser.Pattern := '(in|out|ref)?\s*(((\w+\.)*\w+)(\[\d*\])?)\s?(\w+)(=[^,]*)?(,)?';

  Parser.Global := False;

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  for I := 0 to MatchCollection.Count - 1 do
    begin
      Match := MatchCollection[I] as IMatch2;
      Token := Match.Value;
      Submatches := Match.SubMatches as ISubMatches;
      ParamName := Submatches[5];
      Routine.AddParam(pkByVal, ParamName, TypeName, Default);
    end;
end;

/// Parse parameters for methods
function ParseTypeDeclaration(Routine: TRoutineDeclaration; const AText: string): TParamType;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  I: Integer;
  Text: string;
begin
  Parser := CoRegExp.Create;
  Parser.Global := True; Parser.IgnoreCase := True;

  Parser.Pattern := '(in|out|ref)?\s*(((\w+\.)*\w+)(\[\d*\])?)\s?(\w+)(=[^,]*)?(,)?';

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  for I := 0 to MatchCollection.Count - 1 do
    begin
      Match := MatchCollection[I] as IMatch2;
      Text := Match.Value;
      if Text <> '' then
        ParseParamNames(Routine, Text);
    end;
end;

function ParseReturnType(const AText: string): string;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
begin
  Result := '';
  Parser := CoRegExp.Create;
  Parser.Global := True;

  // Look for one of the following
  // 1) : [Namespace.[Namespace. ...]]Type
  
  Parser.Pattern := '((\w+)\.)*\w+';

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  if MatchCollection.Count = 0 then
    exit;
  Match := MatchCollection.Item[0] as IMatch2;
  Result := Match.Value;
end;

function Parse(const AText: string): TRoutineDeclaration;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  Submatches: ISubMatches;
  Pattern, ReturnType, 
    IsClass, ClassName, RoutineType, RoutineName, Text: string;
  Routine: TRoutineDeclaration;
begin
  Routine := nil;
  Parser := CoRegExp.Create;
  try
    Parser.IgnoreCase := True;
    Parser.Global := False;
    Pattern :=
    '(((new|public|protected|internal|private|static|'+
    'virtual|sealed|override|abstract|extern)*\s)*)'+         // Visibility
    '((\w+\.)*(\w+))'+            // Return type, either a local type, or a namespace type
    '\s?'+
    '(\w*)'+                                   // Method name
    '\('+                                      // Left bracket
    '([^\)]*)'+                                // optional parameters
    '\)';                                      // right bracket

    Parser.Pattern := Pattern;
    if Parser.Test(AText) then // try method parsing
      begin
        MatchCollection := Parser.Execute(AText) as IMatchCollection2;
        Match := MatchCollection.Item[0] as IMatch2;
        Submatches := Match.SubMatches as ISubMatches;

        IsClass := '';
        ReturnType := Submatches[3];
        RoutineType := ReturnType;
        RoutineName := Submatches[6];

        // Check for constructor
        if (ReturnType <> '') and (RoutineName = '') then
          ReturnType := '';

        Routine := TCSharpRoutineDeclaration.Create(IsClass, RoutineType,
              ClassName, RoutineName, ReturnType);

        if Submatches.Count > 7 then
          begin
            Text := Submatches[7];
            if Length(Text) > 0 then
              begin
                ParseTypeDeclaration(Routine, Text);
              end;
          end;

      end else
      begin // try parameter parsing
        Routine := TRoutineDeclaration.Create('', '', '', '', '');
        // raise Exception.CreateFmt('Unable to parse declaration: %s', [AText]);
      end;
  finally
    Result := Routine;
  end;
end;

function ParseCSharpDeclaration(const Declaration: string): string;
var
  RoutineDeclaration: TRoutineDeclaration;
begin
  RoutineDeclaration := Parse(StripSpaces(Declaration));
  Result := RoutineDeclaration.XML;
  RoutineDeclaration.Free;
end;

procedure TestVarDeclaration;
begin
  ParseVarDeclaration('private System.Windows.Forms.Panel panel1;');
  ParseVarDeclaration(StripSpaces('private System.Windows.Forms.Panel panel1, x;'));
end;

procedure TestParser;

  procedure TestParse(const AText: string);
  var
    RoutineDecl: TRoutineDeclaration;
  begin
    if IsConsole then
      begin
        WriteLn(AText);
        RoutineDecl := Parse(StripSpaces(AText));
        WriteLn(RoutineDecl.XML);
        RoutineDecl.Free;
        ReadLn;
      end;
  end;

begin
  if not IsConsole then exit;
  TestParse('private System.Windows.Forms.MainMenu mainMenu1;');
  TestParse('public override void IDERegister(out string[] aMenuNames, out int[] aMenuShortCuts)');
  TestParse('protected override void Dispose (bool disposing)');
  TestParse('public frmRemoteBossClient()');
  TestParse('private blah1.blah2.blah3 WinForm3_Load()');
  TestParse('public static blah1.blah2.blah3 WinForm3_Load(object sender)');
  TestParse('public static blah1 WinForm3_Load(object sender)');
  TestParse('public static void WinForm3_Load(object sender)');
  TestParse('private void WinForm3_Load(object sender, System.EventArgs e)');
end;

initialization
  TestParser;
finalization
end.
