// Productivity Experts by Chua Chee Wee, Singapore
//
// Chua Chee Wee,
// Singapore
// 19 April 2005.
//
// Blog: http://chuacw.ath.cx/chuacw/category/78.aspx

unit XMLCommentsParserImpl;

interface

uses
  SysUtils;

// References
// 1)  ...use regular expressions in Delphi? - http://www.swissdelphicenter.ch/torry/printcode.php?id=1478
// 2) Introduction to Regular Expressions - http://msdn.microsoft.com/library/default.asp?url=/library/en-us/script56/html/reconIntroductionToRegularExpressions.asp

type
  TRoutineType = (rtFunction, rtProcedure, rtMethod, rtConstructor, rtDestructor, rtOperator,
                  rtGetter, rtSetter);
// consider operator as a function instead?

  TParamKind = (pkByVal, pkByRef, pkConst);
  TParamType = record
    ParamKind: TParamKind;
    Name, _Type, Default: string;
  end;
  TRoutine = record
    Name, _Type: string;
    Params: array of TParamType;
  end;

  TRoutineDeclaration = class
  protected
    FXML,
      FIsClass, FType, FClassName, FName, FReturnType: string;
    FParams: array of TParamType;
    function GetXML: string;
    function IsFunction(const AText: string): Boolean; virtual;
  public
    procedure AddParam(AParamKind: TParamKind; const AName, AType, ADefault: string);
    constructor Create(const AIsClass, AType, AClassName, AName, AReturnType: string); virtual;
    property XML: string read GetXML;
  end;

function GetIndentation(const AText: string): string;
function WithinCodeCommentsRegion(const AText: string; out Slashes: string): Boolean;
implementation

uses
  ActiveX, Windows,
  DeclParserIntf, TextUtils, CSharpParserImpl;

function GetIndentation(const AText: string): string;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  Submatches: ISubMatches;
begin
  Result := '';
  Parser := CoRegExp.Create;
  Parser.Global := False;

  Parser.Pattern := '^(\s*)(?:(.))';

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  if MatchCollection.Count = 0 then
    exit;
  Match := MatchCollection.Item[0] as IMatch2;
  Submatches := Match.SubMatches as ISubMatches;
  Result := Submatches[0];
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

  Parser.Pattern := '(?:\s*\:\s*)&?(((\w+)\.)*(\w+))';

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  Assert(MatchCollection.Count = 1);
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
  Default := ParseDefaultValue(AText);
  Parser := CoRegExp.Create;
  Parser.IgnoreCase := True;

// Look for identifiers, or identifiers: 
  Parser.Pattern := '&?(((\w+)\.)*\w+)(?:\s*,|:)';

  Parser.Global := True;

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  for I := 0 to MatchCollection.Count - 1 do
    begin
      Match := MatchCollection.Item[I] as IMatch2;
      Token := Match.Value;
      Submatches := Match.SubMatches as ISubMatches;
      ParamName := Submatches[0];
      Routine.AddParam(pkByVal, ParamName, TypeName, Default);
    end;
end;

function ParseSingleDeclaration(Routine: TRoutineDeclaration; const AText: string): TParamType;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  I: Integer;
begin
  Parser := CoRegExp.Create;
  Parser.IgnoreCase := True;
  Parser.Pattern := '((const|var|out)?(?:\s)*&?(\w+)(\s*,\s*\w+)*(?:\s*\:\s*)&?((\w+)\.)*(\w+)(\s*\=\s*(''.*''|\w+|\d+))?)';
  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  for I := 0 to MatchCollection.Count - 1 do
    begin
      Match := MatchCollection.Item[I] as IMatch2;
      ParseParamNames(Routine, Match.Value);
    end;
end;

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

// A single type declaration is something that is followed by (optionally) a semicolon
  Parser.Pattern := '([^;]*)(\;)?';

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  for I := 0 to MatchCollection.Count - 1 do
    begin
      Match := MatchCollection.Item[I] as IMatch2;
      Text := Match.Value;
      if Text <> '' then
        ParseSingleDeclaration(Routine, Text);
    end;
end;

procedure ParseParams(const AText: string);
var
  Parser: IRegExp2;
begin
  Parser := CoRegExp.Create;
  Parser.Pattern := '';
end;

/// <summary>
/// Parses a valid Delphi routine declaration.
/// NOTE: Free the return class after use.
/// </summary>
/// <param name="AText" type="string">Contains one or more lines of text in the editor</param>
/// <returns>TRoutineDeclaration</returns>
function Parse(const AText: string): TRoutineDeclaration;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  Submatches: ISubMatches;
  I: Integer;
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
      '^' +        // beginning of line
      Whitespace +
      '(class)?' + // optional static class
      Whitespace +
//  '((con|de)structor|function|operator|procedure)'+
    '(constructor|destructor|function|operator|procedure)' +
      Whitespace +
      '(\w+\.)*' + // optional class identifier followed by a dot
      '(\w+)' +    // routine name
      Whitespace +
      '((?:\()((?:\;?\s*)(const|var|out)?' +
      Whitespace +
      '&?(\w+)' + //  (optional &) parameter name
      '(,\s*\w+)*' + // additional parameters
      '(?:\s*\:\s*)' + // type separator

// (optional &) (optional scope/namespace) parameter type name
    '&?((\w*)\.)*(\w+)' +

    '(\s*\=\s*(''.*''|\w+|\d+))?' + // default parameters, no set support
      ')*(?:\)))*((?:\:\s*)' + // grr...
      '((\w*)\.)*(\w+))?;';    // optional return type
    Parser.Pattern := Pattern;
    if Parser.Test(AText) then
      begin
        MatchCollection := Parser.Execute(AText) as IMatchCollection2;
        for I := 0 to MatchCollection.Count - 1 do
          begin
            Match := MatchCollection.Item[I] as IMatch2;
            Submatches := Match.SubMatches as ISubMatches;

            IsClass := Submatches[0];
            RoutineType := Submatches[1];
            ClassName := Submatches[2];
            RoutineName := Submatches[3];
            Text := Submatches[14];
            ReturnType := ParseReturnType(Text);

            Routine := TRoutineDeclaration.Create(IsClass, RoutineType,
              ClassName, RoutineName, ReturnType);
            if Submatches.Count > 4 then
              begin
                Text := Submatches.Item[4];
                if Length(Text) > 0 then
                  begin
                    if Text[1] = '(' then Delete(Text, 1, 1);
                    if Text[Length(Text)] = ')' then Delete(Text, Length(Text), 1);
                    ParseTypeDeclaration(Routine, Text);
                  end;
              end;
          end;
      end else
      begin
        raise Exception.CreateFmt('Unable to parse declaration: %s', [AText]);
      end;
  finally
    Result := Routine;
  end;
end;

/// <summary>
/// Returns the number of slashes found in AText
/// </summary>
/// <param name="AText" type="string"></param>
/// <param name="Slashes" type="string"></param>
/// <returns>Boolean</returns>
function WithinCodeCommentsRegion(const AText: string; out Slashes: string): Boolean;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
begin
  Match := nil;
  Parser := CoRegExp.Create;
  // A simple test for whether we're inside the code comments region
  // is by looking at whether the text is beginning of line, some spaces,
  // followed by ///
  Parser.Pattern := '^(?:\s*)?(/{2,3})';
  Parser.IgnoreCase := True;
  Parser.Global := False;
// this cannot be done with a simple .Test, as it gives false results
  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
  if MatchCollection.Count <> 0 then
    begin
      Match := MatchCollection[0] as IMatch2;
      if Assigned(Match.SubMatches) and (ISubMatches(Match.SubMatches).Count>=1) then
        Slashes := ISubMatches(Match.SubMatches)[0] else
        Slashes := Match.Value;
    end;
  Result := Assigned(Match);
end;

{ TRoutineDeclaration }

procedure TRoutineDeclaration.AddParam(AParamKind: TParamKind;
  const AName, AType, ADefault: string);
var
  LParam: TParamType;
begin
  LParam.ParamKind := AParamKind;
  LParam.Name := AName;
// I should use LParam.&Type, but then, I want to give D7 users the same support
// so, I used _Type
  LParam._Type := AType;
  LParam.Default := ADefault;
  SetLength(FParams, Length(FParams) + 1);
  FParams[High(FParams)] := LParam;
end;

constructor TRoutineDeclaration.Create(const AIsClass, AType, AClassName, AName,
  AReturnType: string);
begin
  FIsClass := AIsClass;
  FType := AType;
  FClassName := AClassName;
  FName := AName;
  FReturnType := AReturnType;
end;

function TRoutineDeclaration.GetXML: string;
var
  Data, LParam, Default, Params, Return: string;
  I: Integer;
begin
  if FXML = '' then
    begin
      Data := '/// <summary>'#13#10 +
        '/// '#13#10 +
        '/// </summary>'#13#10;
      if IsFunction(Lowercase(FType)) then
        Return := Format('/// <returns>%s</returns>'#13#10, [FReturnType]) else
        Return := '';
      Params := '';
      for I := Low(FParams) to High(FParams) do
        begin
          if FParams[I].Default <> '' then
            Default := Format(' default="%s"', [FParams[I].Default]) else
            Default := '';
          LParam := Format('/// <param name="%s" type="%s"%s></param>'#13#10,
            [FParams[I].Name, FParams[I]._Type, Default]);
          Params := Format('%s%s', [Params, LParam]);
        end;
      FXML := Format('%s%s%s', [Data, Params, Return]);
    end;
  Result := FXML;
end;

function TRoutineDeclaration.IsFunction(const AText: string): Boolean;
begin
  Result := (AText = 'function');
end;

initialization
  CoInitialize(nil);
finalization
  CoUninitialize;
end.

