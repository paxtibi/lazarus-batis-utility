unit lzbatis.om.pascal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lzBatis.om.DB, fgl;

type
  TOMCompilationUnit = class;
  TOMInterface = class;
  TOMClass = class;
  TOMMapper = class;
  TOMField = class;
  TOMProperty = class;
  TOMMethod = class;
  TOMParameter = class;
  TOMNamedItem = class;

  TOMNamedItems = specialize TFPGObjectList<TOMNamedItem>;
  TOMMappers = specialize TFPGObjectList<TOMMapper>;
  TOMClasses = specialize TFPGObjectList<TOMClass>;
  TOMInterfaces = specialize TFPGObjectList<TOMInterface>;
  TOMFields = specialize TFPGObjectList<TOMField>;
  TOMMethods = specialize TFPGObjectList<TOMMethod>;
  TOMParameters = specialize TFPGObjectList<TOMParameter>;
  TOMCompilationUnits = specialize TFPGObjectList<TOMCompilationUnit>;
  TOMProperties = specialize TFPGObjectList<TOMProperty>;

  TVisibleLevel = (vlUnknow, vlPublished, vlPublic, vlProtected, vlPrivate);
  TParameterProtocol = (ppCopy, ppConst, ppVar, ppOut);

type
  { TOMNamedItem }
  TOMNamedItem = class
  private
    FName: string;
    procedure SetName(AValue: string);
  public
    constructor Create(aName: string = ''); overload;
    constructor CreateCopy(toCopy: TOMNamedItem); overload;
    property Name: string read FName write SetName;
  end;

  { TOMVisibleItem }

  TOMVisibleItem = class(TOMNamedItem)
  private
    FVisibility: TVisibleLevel;
    procedure SetVisibility(AValue: TVisibleLevel);
  public
    constructor CreateCopy(toCopy: TOMVisibleItem); overload;
    property Visibility: TVisibleLevel read FVisibility write SetVisibility;
    function getVisibilityName: string;
  end;

  { TOMParameter }

  TOMParameter = class(TOMNamedItem)
  private
    FParameterProtocolo: TParameterProtocol;
    FParameterType: TOMNamedItem;
    FParameterTypeName: string;
    procedure SetParameterProtocolo(AValue: TParameterProtocol);
    procedure SetParameterType(AValue: TOMNamedItem);
    procedure SetParameterTypeName(AValue: string);
  public
    property ParameterProtocolo: TParameterProtocol read FParameterProtocolo write SetParameterProtocolo;
    property ParameterType: TOMNamedItem read FParameterType write SetParameterType;
    property ParameterTypeName: string read FParameterTypeName write SetParameterTypeName;
  end;

  { TOMField }

  TOMField = class(TOMVisibleItem)
  private
    FInitializiationValue: string;
    FReferencedColumn: TOMColumn;
    FReferencedType: TOMNamedItem;
    FTypeName: string;
    procedure SetInitializiationValue(AValue: string);
    procedure SetReferencedColumn(AValue: TOMColumn);
    procedure SetReferencedType(AValue: TOMNamedItem);
    procedure SetTypeName(AValue: string);
  public
    property ReferencedType: TOMNamedItem read FReferencedType write SetReferencedType;
    property TypeName: string read FTypeName write SetTypeName;
    property ReferencedColumn: TOMColumn read FReferencedColumn write SetReferencedColumn;
    property InitializiationValue: string read FInitializiationValue write SetInitializiationValue;
  end;

  { TOMMethod }

  TOMMethod = class(TOMVisibleItem)
  private
    FBody: string;
    FisScalar: boolean;
    FVector: boolean;
    FParameters: TOMParameters;
    FResultName: string;
    FReturnTypes: TOMNamedItem;
    FSetterOf: TOMField;
    function GetParameters: TOMParameters;
    procedure SetBody(AValue: string);
    procedure SetisScalar(AValue: boolean);
    procedure SetVector(AValue: boolean);
    procedure SetParameters(AValue: TOMParameters);
    procedure SetResultName(AValue: string);
    procedure SetReturnTypes(AValue: TOMNamedItem);
    procedure SetSetterOf(AValue: TOMField);
  public
    constructor Create; overload;
    destructor Destroy; override;
    property Parameters: TOMParameters read GetParameters write SetParameters;
    property ReturnType: TOMNamedItem read FReturnTypes write SetReturnTypes;
    property ResultName: string read FResultName write SetResultName;
    property SetterOf: TOMField read FSetterOf write SetSetterOf;
    property Body: string read FBody write SetBody;
    property isVector: boolean read FVector write SetVector;
    property isScalar: boolean read FisScalar write SetisScalar;
  end;

  { TOMProperty }

  TOMProperty = class(TOMNamedItem)
  private
    FGetter: TOMMethod;
    FSetter: TOMMethod;
    procedure SetGetter(AValue: TOMMethod);
    procedure SetSetter(AValue: TOMMethod);
  published
    property Setter: TOMMethod read FSetter write SetSetter;
    property Getter: TOMMethod read FGetter write SetGetter;
  end;


  { TOMCompilationUnit }

  TOMCompilationUnit = class(TOMNamedItem)
  private
    FClasses: TOMClasses;
    FCopyright: string;
    FFileName: string;
    FInterfaces: TOMInterfaces;
    FMappers: TOMMappers;
    FPrivateDependences: TOMNamedItems;
    FPublicDependences: TOMNamedItems;
    FUnitName: string;
    function GetClasses: TOMClasses;
    function GetInterfaces: TOMInterfaces;
    function GetMappers: TOMMappers;
    function getPrivateDependences: TOMNamedItems;
    function getPublicDependences: TOMNamedItems;
    procedure SetClasses(AValue: TOMClasses);
    procedure SetCopyright(AValue: string);
    procedure SetFileName(AValue: string);
    procedure SetInterfaces(AValue: TOMInterfaces);
    procedure SetMappers(AValue: TOMMappers);
    procedure SetPrivateDependences(AValue: TOMNamedItems);
    procedure SetPublicDependences(AValue: TOMNamedItems);
    procedure SetUnitName(AValue: string);
  public
    property FileName: string read FFileName write SetFileName;
    property UnitName: string read FUnitName write SetUnitName;
    property Classes: TOMClasses read GetClasses write SetClasses;
    property Mappers: TOMMappers read GetMappers write SetMappers;
    property Interfaces: TOMInterfaces read GetInterfaces write SetInterfaces;
    property PublicDependences: TOMNamedItems read getPublicDependences write SetPublicDependences;
    property PrivateDependences: TOMNamedItems read getPrivateDependences write SetPrivateDependences;
    function getInterfaceByName(const aName: string): TOMInterface;
    function getClassByName(const aName: string): TOMClass;
    function getMapperByName(const aName: string): TOMMapper;
    property Copyright: string read FCopyright write SetCopyright;
  end;


  { TOMAggregateItem }

  TOMAggregateItem = class(TOMVisibleItem)
  private
    FFields: TOMFields;
    FMethods: TOMMethods;
    function GetFields: TOMFields;
    function GetMethods: TOMMethods;
    procedure SetFields(AValue: TOMFields);
    procedure SetMethods(AValue: TOMMethods);
  public
    constructor Create; overload;
    constructor CreateCopy(toCopy: TOMAggregateItem); overload;
    property Fields: TOMFields read GetFields write SetFields;
    property Methods: TOMMethods read GetMethods write SetMethods;
  end;

  { TOMInterface }

  TOMInterface = class(TOMAggregateItem)
  private
    FConcreteClass: TOMNamedItem;
    FGenericName: string;
    FGUID: string;
    FProperties: TOMProperties;
    function GenerateGuid: string;
    function GetProperties: TOMProperties;
    procedure SetConcreteClass(AValue: TOMNamedItem);
    procedure SetGenericName(AValue: string);
  public
    constructor Create(aName: string = ''); overload;
    constructor Create; overload;
    destructor Destroy; override;
    property GUID: string read GenerateGuid;
    property Properties: TOMProperties read GetProperties;
    property ConcreteClass: TOMNamedItem read FConcreteClass write SetConcreteClass;
    property GenericName: string read FGenericName write SetGenericName;
  end;

  { TOMClass }

  TOMClass = class(TOMAggregateItem)
  private
    FImpls: TOMInterfaces;
    function GetImpls: TOMInterfaces;
  public
    destructor Destroy; override;
    property Impls: TOMInterfaces read GetImpls;
  end;

  { TOMMapper }

  TOMMapper = class(TOMAggregateItem)
  private
    FContextName: string;
    procedure SetContextName(AValue: string);
  public
    property ContextName: string read FContextName write SetContextName;
  end;

implementation

{ TOMMapper }

procedure TOMMapper.SetContextName(AValue: string);
begin
  if FContextName = AValue then
  begin
    Exit;
  end;
  FContextName := AValue;
end;

{ TOMClass }

function TOMClass.GetImpls: TOMInterfaces;
begin
  if (FImpls = nil) then
  begin
    FImpls := TOMInterfaces.Create(False);
  end;
  Result := FImpls;
end;

destructor TOMClass.Destroy;
begin
  inherited Destroy;
end;


{ TOMProperty }

procedure TOMProperty.SetGetter(AValue: TOMMethod);
begin
  if FGetter = AValue then
  begin
    Exit;
  end;
  FGetter := AValue;
end;

procedure TOMProperty.SetSetter(AValue: TOMMethod);
begin
  if FSetter = AValue then
  begin
    Exit;
  end;
  FSetter := AValue;
end;

{ TOMInterface }

function TOMInterface.GenerateGuid: string;
var
  Value: TGuid;
begin
  if FGUID = '' then
  begin
    CreateGUID(Value);
    FGUID := GUIDToString(Value);
  end;
  Result := FGUID;
end;

function TOMInterface.GetProperties: TOMProperties;
begin
  if FProperties = nil then
  begin
    FProperties := TOMProperties.Create(True);
  end;
  Result := FProperties;
end;

procedure TOMInterface.SetConcreteClass(AValue: TOMNamedItem);
begin
  if FConcreteClass = AValue then
  begin
    Exit;
  end;
  FConcreteClass := AValue;
end;

procedure TOMInterface.SetGenericName(AValue: string);
begin
  if FGenericName = AValue then
  begin
    Exit;
  end;
  FGenericName := AValue;
end;

constructor TOMInterface.Create(aName: string);
begin
  inherited Create(aName);
  if aName <> '' then
  begin
    FGenericName := 'T' + aName + 'List';
    if aName[1] = 'I' then
    begin
      Delete(FGenericName, 2, 1);
    end;
  end;
end;

constructor TOMInterface.Create;
begin
  inherited Create;
end;

destructor TOMInterface.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

{ TOMAggregateItem }

procedure TOMAggregateItem.SetFields(AValue: TOMFields);
begin
  if FFields = AValue then
  begin
    Exit;
  end;
  FFields := AValue;
end;

function TOMAggregateItem.GetFields: TOMFields;
begin
  if FFields = nil then
  begin
    FFields := TOMFields.Create(True);
  end;
  Result := FFields;
end;

function TOMAggregateItem.GetMethods: TOMMethods;
begin
  if FMethods = nil then
  begin
    FMethods := TOMMethods.Create(True);
  end;
  Result := FMethods;
end;

procedure TOMAggregateItem.SetMethods(AValue: TOMMethods);
begin
  if FMethods = AValue then
  begin
    Exit;
  end;
  FMethods := AValue;
end;

constructor TOMAggregateItem.Create;
begin
  FFields := nil;
  FMethods := nil;
end;

constructor TOMAggregateItem.CreateCopy(toCopy: TOMAggregateItem);
var
  omF: TOMField;
  omM: TOMMethod;
begin
  inherited CreateCopy(toCopy);
  for omf in toCopy.Fields do
  begin
    Fields.Add(TOMField.CreateCopy(omf));
  end;
  for omm in toCopy.Methods do
  begin
    Methods.Add(TOMMethod.CreateCopy(omm));
  end;
end;

{ TOMField }

procedure TOMField.SetReferencedType(AValue: TOMNamedItem);
begin
  if FReferencedType = AValue then
  begin
    Exit;
  end;
  FReferencedType := AValue;
end;

procedure TOMField.SetTypeName(AValue: string);
begin
  if FTypeName = AValue then
  begin
    Exit;
  end;
  FTypeName := AValue;
end;

procedure TOMField.SetReferencedColumn(AValue: TOMColumn);
begin
  if FReferencedColumn = AValue then
  begin
    Exit;
  end;
  FReferencedColumn := AValue;
end;

procedure TOMField.SetInitializiationValue(AValue: string);
begin
  if FInitializiationValue = AValue then
  begin
    Exit;
  end;
  FInitializiationValue := AValue;
end;

{ TOMMethod }

procedure TOMMethod.SetParameters(AValue: TOMParameters);
begin
  if FParameters = AValue then
  begin
    Exit;
  end;
  FParameters := AValue;
end;

procedure TOMMethod.SetResultName(AValue: string);
begin
  if FResultName = AValue then
  begin
    Exit;
  end;
  FResultName := AValue;
end;

function TOMMethod.GetParameters: TOMParameters;
begin
  if FParameters = nil then
  begin
    FParameters := TOMParameters.Create(True);
  end;
  Result := FParameters;
end;

procedure TOMMethod.SetBody(AValue: string);
begin
  if FBody = AValue then
  begin
    Exit;
  end;
  FBody := AValue;
end;

procedure TOMMethod.SetisScalar(AValue: boolean);
begin
  if FisScalar = AValue then
    Exit;
  FisScalar := AValue;
end;

procedure TOMMethod.SetVector(AValue: boolean);
begin
  if FVector = AValue then
  begin
    Exit;
  end;
  FVector := AValue;
end;

procedure TOMMethod.SetReturnTypes(AValue: TOMNamedItem);
begin
  if FReturnTypes = AValue then
  begin
    Exit;
  end;
  FReturnTypes := AValue;
  if FReturnTypes <> nil then
  begin
    FResultName := FReturnTypes.Name;
  end
  else
  begin
    FResultName := '';
  end;
end;

procedure TOMMethod.SetSetterOf(AValue: TOMField);
begin
  if FSetterOf = AValue then
  begin
    Exit;
  end;
  FSetterOf := AValue;
end;

constructor TOMMethod.Create;
begin
  FParameters := nil;
  FVector := False;
end;

destructor TOMMethod.Destroy;
begin
  FreeAndNil(FParameters);
  inherited Destroy;
end;

{ TOMParameter }

procedure TOMParameter.SetParameterProtocolo(AValue: TParameterProtocol);
begin
  if FParameterProtocolo = AValue then
  begin
    Exit;
  end;
  FParameterProtocolo := AValue;
end;

procedure TOMParameter.SetParameterType(AValue: TOMNamedItem);
begin
  if FParameterType = AValue then
  begin
    Exit;
  end;
  FParameterType := AValue;
end;

procedure TOMParameter.SetParameterTypeName(AValue: string);
begin
  if FParameterTypeName = AValue then
  begin
    Exit;
  end;
  FParameterTypeName := AValue;
end;


{ TOMNamedItem }

procedure TOMNamedItem.SetName(AValue: string);
begin
  if FName = AValue then
  begin
    Exit;
  end;
  FName := AValue;
end;

constructor TOMNamedItem.Create(aName: string);
begin
  inherited Create;
  FName := aName;
end;

constructor TOMNamedItem.CreateCopy(toCopy: TOMNamedItem);
begin
  inherited Create;
  FName := toCopy.Name;
end;

{ TOMVisibleItem }

procedure TOMVisibleItem.SetVisibility(AValue: TVisibleLevel);
begin
  if FVisibility = AValue then
  begin
    Exit;
  end;
  FVisibility := AValue;
end;

constructor TOMVisibleItem.CreateCopy(toCopy: TOMVisibleItem);
begin
  inherited CreateCopy(toCopy);
  FVisibility := toCopy.Visibility;
end;

function TOMVisibleItem.getVisibilityName: string;
begin
  Result := '';
  case FVisibility of
    vlPublished:
    begin
      Result := 'published';
    end;
    vlPublic:
    begin
      Result := 'public';
    end;
    vlProtected:
    begin
      Result := 'protected';
    end;
    vlPrivate:
    begin
      Result := 'private';
    end;
  end;
end;

{ TOMCompilationUnit }

procedure TOMCompilationUnit.SetFileName(AValue: string);
begin
  if FFileName = AValue then
  begin
    Exit;
  end;
  FFileName := AValue;
end;

procedure TOMCompilationUnit.SetClasses(AValue: TOMClasses);
begin
  if FClasses = AValue then
  begin
    Exit;
  end;
  FClasses := AValue;
end;

procedure TOMCompilationUnit.SetCopyright(AValue: string);
begin
  if FCopyright = AValue then
  begin
    Exit;
  end;
  FCopyright := AValue;
end;

function TOMCompilationUnit.GetClasses: TOMClasses;
begin
  if FClasses = nil then
  begin
    FClasses := TOMClasses.Create(True);
  end;
  Result := FClasses;
end;

function TOMCompilationUnit.GetInterfaces: TOMInterfaces;
begin
  if FInterfaces = nil then
  begin
    FInterfaces := TOMInterfaces.Create(True);
  end;
  Result := FInterfaces;
end;

function TOMCompilationUnit.GetMappers: TOMMappers;
begin
  if FMappers = nil then
  begin
    FMappers := TOMMappers.Create(True);
  end;
  Result := FMappers;
end;

function TOMCompilationUnit.getPrivateDependences: TOMNamedItems;
begin
  if FPrivateDependences = nil then
  begin
    FPrivateDependences := TOMNamedItems.Create(True);
  end;
  Result := FPrivateDependences;
end;

function TOMCompilationUnit.getPublicDependences: TOMNamedItems;
begin
  if FPublicDependences = nil then
  begin
    FPublicDependences := TOMNamedItems.Create(True);
  end;
  Result := FPublicDependences;
end;

procedure TOMCompilationUnit.SetInterfaces(AValue: TOMInterfaces);
begin
  if FInterfaces = AValue then
  begin
    Exit;
  end;
  FInterfaces := AValue;
end;

procedure TOMCompilationUnit.SetMappers(AValue: TOMMappers);
begin
  if FMappers = AValue then
  begin
    Exit;
  end;
  FMappers := AValue;
end;

procedure TOMCompilationUnit.SetPrivateDependences(AValue: TOMNamedItems);
begin
  if FPrivateDependences = AValue then
  begin
    Exit;
  end;
  FPrivateDependences := AValue;
end;

procedure TOMCompilationUnit.SetPublicDependences(AValue: TOMNamedItems);
begin
  if FPublicDependences = AValue then
  begin
    Exit;
  end;
  FPublicDependences := AValue;
end;

procedure TOMCompilationUnit.SetUnitName(AValue: string);
begin
  if FUnitName = AValue then
  begin
    Exit;
  end;
  FUnitName := AValue;
end;

function TOMCompilationUnit.getInterfaceByName(const aName: string): TOMInterface;
var
  cursor: TOMInterface;
begin
  Result := nil;
  for cursor in Interfaces do
  begin
    if cursor.Name = aName then
    begin
      exit(cursor);
    end;
  end;
end;

function TOMCompilationUnit.getClassByName(const aName: string): TOMClass;
var
  cursor: TOMClass;
begin
  Result := nil;
  for cursor in Classes do
  begin
    if cursor.Name = aName then
    begin
      exit(cursor);
    end;
  end;
end;

function TOMCompilationUnit.getMapperByName(const aName: string): TOMMapper;
var
  cursor: TOMMapper;
begin
  Result := nil;
  for cursor in Mappers do
  begin
    if cursor.Name = aName then
    begin
      exit(cursor);
    end;
  end;
end;


end.
