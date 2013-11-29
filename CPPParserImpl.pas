unit CPPParserImpl;

// Recursive matching reference: http://www.pcre.org/pcre.txt

interface

function ParseCPPDeclaration(const Declaration: string): string;
procedure TestCPPParser;

implementation
  uses SysUtils, XMLCommentsParserImpl, DeclParserIntf, TextUtils;

type
  TCPPRoutineDeclaration = class(TRoutineDeclaration)
  public
    constructor Create(const AIsClass, AType, AClassName, AName, AReturnType: string); override;
  end;

constructor TCPPRoutineDeclaration.Create(const AIsClass, AType, AClassName, AName, AReturnType: string);
var
  LClassName: string;
begin
  if Pos('::', AClassName) = Length(AClassName)-1 then
    LClassName := Copy(AClassName, 1, Length(AClassName)-2);
  inherited Create(AIsClass, AType, LClassName, AName, AReturnType);
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
  Parser.Pattern := '\s*\=\s*(''.*''|\w+|\d+)';

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

// A single type declaration is something that is followed by (optionally) a comma
  Parser.Pattern := '([^,]*)(\,)?';

  MatchCollection := Parser.Execute(AText) as IMatchCollection2;
// Check for balanced brackets, eg 1 ( must be closed by 1 )

  for I := 0 to MatchCollection.Count - 1 do
    begin
      Match := MatchCollection.Item[I] as IMatch2;
      Text := Match.Value;
      if Text <> '' then
        ParseSingleDeclaration(Routine, Text);
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

/// <summary>
/// Parses a valid C++ routine declaration.
/// NOTE: Free the return class after use.
/// </summary>
/// <param name="AText" type="string">Contains one or more lines of text in the editor</param>
/// <returns>TRoutineDeclaration</returns>
function ParseCPP(const AText: string): TRoutineDeclaration;
var
  Parser: IRegExp2;
  MatchCollection: IMatchCollection2;
  Match: IMatch2;
  Submatches: ISubMatches;
  I: Integer;
  Pattern, ReturnType, StrippedText,
    IsClass, ClassName, RoutineType, RoutineName, Text: string;
  Routine: TRoutineDeclaration;
begin
  Routine := nil;
  Parser := CoRegExp.Create;
  try
    Parser.IgnoreCase := True;
    Parser.Global := False;
    Pattern :=
    '(\w+)?' +           // (optional) typename
    Whitespace +
    '((\w+\:\:)|(~))?'+  // classname or destructor
    '(\w+)'+             // function name
    '(\((.*)(,(\.\.\.)\s*)*\))' + // parameters
    Whitespace+
    '((const|volatile)(\s*)?){0,2}';
    
    Parser.Pattern := Pattern;
    StrippedText := StripSpaces(AText);
    if Parser.Test(StrippedText) then
      begin
        MatchCollection := Parser.Execute(StrippedText) as IMatchCollection2;
        for I := 0 to MatchCollection.Count - 1 do
          begin
            Match := MatchCollection.Item[I] as IMatch2;
            Submatches := Match.SubMatches as ISubMatches;

            ReturnType := ParseReturnType(Submatches[0]);
            ClassName := Submatches[1];
            ClassName := Submatches[2];
            RoutineName := Submatches[4];

            Routine := TCPPRoutineDeclaration.Create(IsClass, RoutineType,
              ClassName, RoutineName, ReturnType);

            if Submatches.Count > 5 then
              begin
                Text := Submatches.Item[5];
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

// ~destructorName
/// <summary>
/// Parses a C++ routine declaration
/// </summary>
/// <param name="Declaration" type="string"></param>
/// <returns>string</returns>
function ParseCPPDeclaration(const Declaration: string): string;
var
  RoutineDeclaration: TRoutineDeclaration;
begin
  RoutineDeclaration := ParseCPP(Declaration);
  Result := RoutineDeclaration.XML;
  RoutineDeclaration.Free;
end;

 procedure TestCPPParser;
  procedure TestParse(const AText: string);
  var
    RoutineDecl: TRoutineDeclaration;
  begin
    if IsConsole then
      begin
        WriteLn(AText);
        RoutineDecl := ParseCPP(AText);
        WriteLn(RoutineDecl.XML);
        RoutineDecl.Free;
        ReadLn;
      end;
  end;
begin
  TestParse('int   class::test(   MyClass  x = MyClass( std::string(), std::string() ))');
  TestParse('int    register_callback( void (*)() )');
  TestParse('int test( std::string s = std::string() )');
  TestParse('~test()');
  TestParse('int silly_fun( int ****************deep )');
end;

end.
