unit Json.DTO.Base;

interface

uses System.Classes, System.JSON, CMS.JSON, System.Generics.Collections, CMS.JSONReflect;

type
  TArrayMapper = class
  protected
    procedure RefreshArray<T>(aSource: TList<T>; var aDestination: TArray<T>);
    function List<T>(var aList: TList<T>; aSource: TArray<T>): TList<T>;
    function ObjectList<T: class>(var aList: TObjectList<T>; aSource: TArray<T>): TObjectList<T>;
  public
    constructor Create; virtual;
  end;

  TJsonDTO = class(TArrayMapper)
  private
    FOptions: TJsonOptions;
    class procedure PrettyPrintPair(aJSONValue: TJSONPair; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
    class procedure PrettyPrintJSON(aJSONValue: TJsonValue; aOutputStrings: TStrings; Indent: Integer = 0); overload;
    class procedure PrettyPrintArray(aJSONValue: TJSONArray; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
  protected
    function GetAsJson: string; virtual;
    procedure SetAsJson(aValue: string); virtual;
  public
    constructor Create; override;
    class function PrettyPrintJSON(aJson: string): string; overload;
    function ToString: string; override;
    property AsJson: string read GetAsJson write SetAsJson;
  end;

  GenericListReflectAttribute = class(JsonReflectAttribute)
  public
    constructor Create;
  end;

  SuppressZeroAttribute = class(JsonReflectAttribute)
  public
    constructor Create;
  end;

implementation

uses System.SysUtils, System.JSON.Consts, System.Rtti, System.DateUtils;

{ TJsonDTO }

procedure DTFmtError(AErrorCode: DTErrorCode; const AValue: string);
const
  Errors: array[DTErrorCode] of string = ('INVALID DATE!', 'INVALID TIME!', 'INVALID OFFSET!');
begin
  raise EDateTimeException.CreateFmt(Errors[AErrorCode], [AValue]);
end;

function GetNextDTComp(var P: PChar; const PEnd: PChar; const DefValue: string; Prefix: Char;
IsOptional: Boolean; IsDate: Boolean; ErrorCode: DTErrorCode; const AValue: string; NumDigits: Integer): string; overload;
const
  SEmptySeparator: Char = ' ';
var
  LDigits: Integer;
begin
  if (P >= PEnd) then
  begin
    Result := DefValue;
    Exit;
  end;

  Result := '';

  if (Prefix <> SEmptySeparator) and (IsDate or not (Byte(P^) in [Ord('+'), Ord('-')])) then
  begin
    if P^ <> Prefix then
      DTFmtError(ErrorCode, AValue);
    Inc(P);
  end;

  LDigits := 0;
  while ((P <= PEnd) and (P^ >= '0') and (P^ <= '9') and (LDigits < NumDigits)) do
  begin
    Result := Result + P^;
    Inc(P);
    Inc(LDigits);
  end;

  if Result = '' then
  begin
    if IsOptional then
      Result := DefValue
    else
      DTFmtError(ErrorCode, AValue);
  end;
end;

procedure DecodeISO8601Date(const DateString: string; var AYear, AMonth, ADay: Word);
const
  SDateSeparator: Char = '-';
  SEmptySeparator: Char = ' ';
var
  P, PE: PWideChar;
  LNegativeDate: Boolean;
begin
  AYear := 0;
  AMonth := 1;
  ADay := 1;

  P := PChar(DateString);
  PE := P + (DateString.Length - 1);

  LNegativeDate := P^ = '-';
  if LNegativeDate then
    Inc(P);

  if PE - P + 1 < 4 then
    DTFmtError(DTErrorCode.InvDate, DateString);

  if DateString.IndexOf(SDateSeparator) < 0 then
  begin
    AYear := StrToInt(GetNextDTComp(P, PE, '1900', SEmptySeparator, True, True, DTErrorCode.InvDate, DateString, 4));
    AMonth := StrToInt(GetNextDTComp(P, PE, '00', SEmptySeparator, True, True, DTErrorCode.InvDate, DateString, 2));
    ADay := StrToInt(GetNextDTComp(P, PE, '00', SEmptySeparator, True, True, DTErrorCode.InvDate, DateString, 2));
  end
  else
  begin
    AYear := StrToInt(GetNextDTComp(P, PE, '1900', SEmptySeparator, True, True, DTErrorCode.InvDate, DateString, 4));
    AMonth := StrToInt(GetNextDTComp(P, PE, '00', SDateSeparator, True, True, DTErrorCode.InvDate, DateString, 2));
    ADay := StrToInt(GetNextDTComp(P, PE, '00', SDateSeparator, True, True, DTErrorCode.InvDate, DateString, 2));
  end;

  if P <= PE then
    DTFmtError(DTErrorCode.InvDate, DateString);
end;

procedure DecodeISO8601Time(const TimeString: string; var AHour, AMinute, ASecond, AMillisecond: Word;
var AHourOffset, AMinuteOffset: Integer);
const
  SEmptySeparator: Char = ' ';
  STimeSeparator: Char = ':';
  SMilSecSeparator: Char = '.';
var
  LFractionalSecondString: string;
  P, PE: PChar;
  LOffsetSign: Char;
  LFraction: Double;
begin
  AHour := 0;
  AMinute := 0;
  ASecond := 0;
  AMillisecond := 0;
  AHourOffset := 0;
  AMinuteOffset := 0;
  if TimeString <> '' then
  begin
    P := PChar(TimeString);
    PE := P + (TimeString.Length - 1);
    if TimeString.IndexOf(STimeSeparator) < 0 then
    begin
      AHour := StrToInt(GetNextDTComp(P, PE, '00', SEmptySeparator, False, False, DTErrorCode.InvTime, TimeString, 2));
      AMinute := StrToInt(GetNextDTComp(P, PE, '00', SEmptySeparator, False, False, DTErrorCode.InvTime, TimeString, 2));
      ASecond := StrToInt(GetNextDTComp(P, PE, '00', SEmptySeparator, True, False, DTErrorCode.InvTime, TimeString, 2));
      LFractionalSecondString := GetNextDTComp(P, PE, '0', SMilSecSeparator, True, False, DTErrorCode.InvTime, TimeString, 18);
      if LFractionalSecondString <> '0' then
      begin
        LFractionalSecondString := FormatSettings.DecimalSeparator + LFractionalSecondString;
        LFraction := LFractionalSecondString.ToDouble;
        AMillisecond := Round(1000 * LFraction);
      end;
    end
    else
    begin
      AHour := StrToInt(GetNextDTComp(P, PE, '00', SEmptySeparator, False, False, DTErrorCode.InvTime, TimeString, 2));
      AMinute := StrToInt(GetNextDTComp(P, PE, '00', STimeSeparator, False, False, DTErrorCode.InvTime, TimeString, 2));
      ASecond := StrToInt(GetNextDTComp(P, PE, '00', STimeSeparator, True, False, DTErrorCode.InvTime, TimeString, 2));
      LFractionalSecondString := GetNextDTComp(P, PE, '0', SMilSecSeparator, True, False, DTErrorCode.InvTime, TimeString, 18);
      if LFractionalSecondString <> '0' then
      begin
        LFractionalSecondString := FormatSettings.DecimalSeparator + LFractionalSecondString;
        LFraction := LFractionalSecondString.ToDouble;
        AMillisecond := Round(1000 * LFraction);
      end;
    end;

    if (P^ = '-') or (P^ = '+') then
    begin
      LOffsetSign := P^;
      Inc(P);
      if not ((P^ >= '0') and (P^ <= '9')) then
        DTFmtError(DTErrorCode.InvTime, TimeString);
      AHourOffset := StrToInt(LOffsetSign + GetNextDTComp(P, PE, '00', STimeSeparator, True, True, DTErrorCode.InvOffset, TimeString, 2));
      if P^ = ':' then
        AMinuteOffset := StrToInt(LOffsetSign + GetNextDTComp(P, PE, '00', STimeSeparator, True, True, DTErrorCode.InvOffset, TimeString, 2))
      else
        AMinuteOffset := StrToInt(LOffsetSign + GetNextDTComp(P, PE, '00', SEmptySeparator, True, True, DTErrorCode.InvOffset, TimeString, 2));
    end
    else if P^ = 'Z' then
      Inc(P);

    if P <= PE then
      DTFmtError(DTErrorCode.InvTime, TimeString);
  end;
end;

function AdjustDateTime(const ADate: TDateTime; AHourOffset, AMinuteOffset: Integer; IsUTC: Boolean = True): TDateTime;
var
  AdjustDT: TDateTime;
  BiasLocal: Int64;
  BiasTime: Integer;
  BiasHour: Integer;
  BiasMins: Integer;
  BiasDT: TDateTime;
begin
  Result := ADate;
  if IsUTC then
  begin
    { If we have an offset, adjust time to go back to UTC }
    if (AHourOffset <> 0) or (AMinuteOffset <> 0) then
    begin
      AdjustDT := EncodeTime(Abs(AHourOffset), Abs(AMinuteOffset), 0, 0);
      if ((AHourOffset * MinsPerHour) + AMinuteOffset) > 0 then
        Result := Result - AdjustDT
      else
        Result := Result + AdjustDT;
    end;
  end
  else
  begin
    { If we does not have an offset, adjust time to go back to local }
    if (AHourOffset = 0) and (AMinuteOffset = 0) then
      Result := TTimeZone.Local.ToLocalTime(ADate)
    else
    begin
      { Now adjust TDateTime based on any offsets we have and the local bias }
      { There are two possibilities:
          a. The time we have has the same offset as the local bias - nothing to do!!
          b. The time we have and the local bias are different - requiring adjustments }
      BiasLocal := Trunc(TTimeZone.Local.GetUTCOffset(Result).Negate.TotalMinutes);
      BiasTime  := (AHourOffset * MinsPerHour) + AMinuteOffset;
      if (BiasLocal + BiasTime) = 0 then
        Exit;

      { Here we adjust the Local Bias to make it relative to the Time's own offset
        instead of being relative to GMT }
      BiasLocal := BiasLocal + BiasTime;
      BiasHour := Abs(BiasLocal) div MinsPerHour;
      BiasMins := Abs(BiasLocal) mod MinsPerHour;
      BiasDT := EncodeTime(BiasHour, BiasMins, 0, 0);
      if (BiasLocal > 0) then
        Result := Result - BiasDT
      else
        Result := Result + BiasDT;
    end;
  end;
end;

function ISO8601ToDate(const AISODate: string; AReturnUTC: Boolean = True): TDateTime;
const
  STimePrefix: Char = 'T';
var
TimeString, DateString: string;
TimePosition: Integer;
Year, Month, Day, Hour, Minute, Second, Millisecond: Word;
HourOffset, MinuteOffset: Integer;
AddDay, AddMinute, AddSecond: Boolean;
begin
  HourOffset := 0;
  MinuteOffset := 0;
  TimePosition := AISODate.IndexOf(STimePrefix);
  if TimePosition >= 0 then
  begin
    DateString := AISODate.Substring(0, TimePosition);
    TimeString := AISODate.Substring(TimePosition + 1);
  end
  else
  begin
    Hour := 0;
    Minute := 0;
    Second := 0;
    Millisecond := 0;
    HourOffset := 0;
    MinuteOffset := 0;
    DateString := AISODate;
    TimeString := '';
  end;
  DecodeISO8601Date(DateString, Year, Month, Day);
  DecodeISO8601Time(TimeString, Hour, Minute, Second, Millisecond, HourOffset, MinuteOffset);

  AddDay := Hour = 24;
  if AddDay then
    Hour := 0;

  AddMinute := Second = 60;
  if AddMinute then
    Second := 0;

  AddSecond := Millisecond = 1000;
  if AddSecond then
    Millisecond := 0;

  Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Millisecond);

  if AddDay then
    Result := IncDay(Result);
  if AddMinute then
    Result := IncMinute(Result);
  if AddSecond then
    Result := IncSecond(Result);

  Result := AdjustDateTime(Result, HourOffset, MinuteOffset, AReturnUTC);
end;

function DateToISO8601(const ADate: TDateTime; AInputIsUTC: Boolean = true): string;
const
  SDateFormat: string = '%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3dZ'; { Do not localize }
  SOffsetFormat: string = '%s%s%.02d:%.02d'; { Do not localize }
  Neg: array[Boolean] of string = ('+', '-'); { Do not localize }
var
  y, mo, d, h, mi, se, ms: Word;
  Bias: Integer;
  TimeZone: TTimeZone;
begin
  DecodeDate(ADate, y, mo, d);
  DecodeTime(ADate, h, mi, se, ms);
  Result := Format(SDateFormat, [y, mo, d, h, mi, se, ms]);
  if not AInputIsUTC then
  begin
    TimeZone := TTimeZone.Local;
    Bias := Trunc(TimeZone.GetUTCOffset(ADate).Negate.TotalMinutes);
    if Bias <> 0 then
    begin
      // Remove the Z, in order to add the UTC_Offset to the string.
      SetLength(Result, Result.Length - 1);
      Result := Format(SOffsetFormat, [Result, Neg[Bias > 0], Abs(Bias) div MinsPerHour,
        Abs(Bias) mod MinsPerHour]);
    end
  end;
end;


constructor TJsonDTO.Create;
begin
  inherited;
  FOptions := [joDateIsUTC, joDateFormatISO8601];
end;

function TJsonDTO.GetAsJson: string;
begin
  Result := TJson.ObjectToJsonString(Self, FOptions);
end;

const
  INDENT_SIZE = 2;

class procedure TJsonDTO.PrettyPrintJSON(aJSONValue: TJsonValue; aOutputStrings: TStrings; Indent: Integer);
var
  i: Integer;
  Ident: Integer;
begin
  Ident := Indent + INDENT_SIZE;
  i := 0;

  if aJSONValue is TJSONObject then
  begin
    aOutputStrings.Add(StringOfChar(' ', Ident) + '{');
    for i := 0 to TJSONObject(aJSONValue).Size - 1 do
      PrettyPrintPair(TJSONObject(aJSONValue).Get(i), aOutputStrings, i = TJSONObject(aJSONValue).Size - 1, Ident);

    aOutputStrings.Add(StringOfChar(' ', Ident) + '}');
  end
  else if aJSONValue is TJSONArray then
    PrettyPrintArray(TJSONArray(aJSONValue), aOutputStrings, i = TJSONObject(aJSONValue).Size - 1, Ident)
  else
    aOutputStrings.Add(StringOfChar(' ', Ident) + aJSONValue.ToString);
end;

class procedure TJsonDTO.PrettyPrintArray(aJSONValue: TJSONArray; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
var
  i: Integer;
begin
  aOutputStrings.Add(StringOfChar(' ', Indent + INDENT_SIZE) + '[');

  for i := 0 to aJSONValue.Size - 1 do
  begin
    PrettyPrintJSON(aJSONValue.Get(i), aOutputStrings, Indent);
    if i < aJSONValue.Size - 1 then
      aOutputStrings[aOutputStrings.Count - 1] := aOutputStrings[aOutputStrings.Count - 1] + ',';
  end;

  aOutputStrings.Add(StringOfChar(' ', Indent + INDENT_SIZE - 2) + ']');
end;

class function TJsonDTO.PrettyPrintJSON(aJson: string): string;
var
  StringList: TStringlist;
  JSONValue: TJsonValue;
begin
  StringList := TStringlist.Create;
  try
    JSONValue := TJSONObject.ParseJSONValue(aJson);
    try
      if JSONValue <> nil then
        PrettyPrintJSON(JSONValue, StringList);
    finally
      JSONValue.Free;
    end;

    Result := StringList.Text;
  finally
    StringList.Free;
  end;
end;

class procedure TJsonDTO.PrettyPrintPair(aJSONValue: TJSONPair; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
const
  TEMPLATE = '%s:%s';
var
  Line: string;
  NewList: TStringlist;
begin
  NewList := TStringlist.Create;
  try
    PrettyPrintJSON(aJSONValue.JSONValue, NewList, Indent);
    Line := Format(TEMPLATE, [aJSONValue.JsonString.ToString, Trim(NewList.Text)]);
  finally
    NewList.Free;
  end;

  Line := StringOfChar(' ', Indent + INDENT_SIZE) + Line;
  if not Last then
    Line := Line + ',';
  aOutputStrings.Add(Line);
end;

procedure TJsonDTO.SetAsJson(aValue: string);
var
  JSONValue: TJsonValue;
  JSONObject: TJSONObject;
begin
  JSONValue := TJSONObject.ParseJSONValue(aValue);
  try
    if not Assigned(JSONValue) then
      Exit;

    if (JSONValue is TJSONArray) then
    begin
      with TJSONUnMarshal.Create do
        try
          SetFieldArray(Self, 'Items', (JSONValue as TJSONArray));
        finally
          Free;
        end;

      Exit;
    end;

    if (JSONValue is TJSONObject) then
      JSONObject := JSONValue as TJSONObject
    else
    begin
      aValue := aValue.Trim;
      if (aValue = '') and not Assigned(JSONValue) or (aValue <> '') and Assigned(JSONValue) and JSONValue.Null then
        Exit
      else
        raise EConversionError.Create(SCannotCreateObject);
    end;

    TJson.JsonToObject(Self, JSONObject, FOptions);
  finally
    JSONValue.Free;
  end;
end;

function TJsonDTO.ToString: string;
begin
  Result := AsJson;
end;

{ TArrayMapper }

constructor TArrayMapper.Create;
begin
  inherited;
end;

function TArrayMapper.List<T>(var aList: TList<T>; aSource: TArray<T>): TList<T>;
begin
  if aList = nil then
  begin
    aList := TList<T>.Create;
    aList.AddRange(aSource);
  end;

  Exit(aList);
end;

function TArrayMapper.ObjectList<T>(var aList: TObjectList<T>; aSource: TArray<T>): TObjectList<T>;
var
  Element: T;
begin
  if aList = nil then
  begin
    aList := TObjectList<T>.Create;
    for Element in aSource do
      aList.Add(Element);
  end;

  Exit(aList);
end;

procedure TArrayMapper.RefreshArray<T>(aSource: TList<T>; var aDestination: TArray<T>);
begin
  if aSource <> nil then
    aDestination := aSource.ToArray;
end;

type
  TGenericListFieldInterceptor = class(TJSONInterceptor)
  public
    function ObjectsConverter(Data: TObject; Field: string): TListOfObjects; override;
  end;

  { TListFieldInterceptor }

function TGenericListFieldInterceptor.ObjectsConverter(Data: TObject; Field: string): TListOfObjects;
var
  ctx: TRttiContext;
  List: TList<TObject>;
  RttiProperty: TRttiProperty;
begin
  RttiProperty := ctx.GetType(Data.ClassInfo).GetProperty(Copy(Field, 2, MAXINT));
  List := TList<TObject>(RttiProperty.GetValue(Data).AsObject);
  Result := TListOfObjects(List.List);
  SetLength(Result, List.Count);
end;

constructor GenericListReflectAttribute.Create;
begin
  inherited Create(ctObjects, rtObjects, TGenericListFieldInterceptor, nil, false);
end;

type
  TSuppressZeroDateInterceptor = class(TJSONInterceptor)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

function TSuppressZeroDateInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  RttiContext: TRttiContext;
  Date: TDateTime;
begin
  Date := RttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TDateTime>;
  if Date = 0 then
    Result := string.Empty
  else
    Result := DateToISO8601(Date, True);
end;

procedure TSuppressZeroDateInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  RttiContext: TRttiContext;
  Date: TDateTime;
begin
  if Arg.IsEmpty then
    Date := 0
  else
    Date := ISO8601ToDate(Arg, True);

  RttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data, Date);
end;

{ SuppressZeroAttribute }

constructor SuppressZeroAttribute.Create;
begin
  inherited Create(ctString, rtString, TSuppressZeroDateInterceptor);
end;

end.
