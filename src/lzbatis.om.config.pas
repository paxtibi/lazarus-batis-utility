unit lzbatis.om.config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, lzBatis.om.pascal;

type
  TConfiguration = class;
  TConfigurationContext = class;
  TConfigurationTable = class;
  TConfigurationColumn = class;
  TConfigurationTypeHandler = class;
  TConfigurationTableForeingKey = class;
  TConfigurationTablePrimaryKey = class;
  TConfigurationMapper = class;
  TConfigurationMapperMethod = class;
  TConfigurationParameter = class;

  TConfigurationContextes = specialize TFPGObjectList<TConfigurationContext>;
  TConfigurationTables = specialize TFPGObjectList<TConfigurationTable>;
  TConfigurationColumns = specialize TFPGObjectList<TConfigurationColumn>;
  TConfigurationTableForeingKeys = specialize TFPGObjectList<TConfigurationTableForeingKey>;
  TConfigurationTablePrimaryKeys = specialize TFPGObjectList<TConfigurationTablePrimaryKey>;
  TConfigurationTypeHandlers = specialize  TFPGObjectList<TConfigurationTypeHandler>;
  TConfigurationParameters = specialize  TFPGObjectList<TConfigurationParameter>;
  TConfigurationMapperMethods = specialize  TFPGObjectList<TConfigurationMapperMethod>;


  { TConfigurationTableForeingKey }

  TConfigurationTableForeingKey = class
  private
    FColumns: TConfigurationColumns;
    function GetColumns: TConfigurationColumns;
  public
    destructor Destroy; override;
    property Columns: TConfigurationColumns read GetColumns;
  end;

  { TConfigurationTablePrimaryKey }

  TConfigurationTablePrimaryKey = class
  private
    FColumns: TConfigurationColumns;
    function GetColumns: TConfigurationColumns;
  public
    destructor Destroy; override;
    property Columns: TConfigurationColumns read GetColumns;
  end;

  { TConfigurationColumn }

  TConfigurationColumn = class
  private
    FColumnName: string;
    FColumnType: string;
    FDefaultVale: string;
    FModelName: string;
    FModelType: string;
    FSkip: boolean;
    procedure SetColumnName(AValue: string);
    procedure SetColumnType(AValue: string);
    procedure SetDefaultVale(AValue: string);
    procedure SetModelName(AValue: string);
    procedure SetModelType(AValue: string);
    procedure SetSkip(AValue: boolean);
  public
    property ColumnName: string read FColumnName write SetColumnName;
    property ModelName: string read FModelName write SetModelName;
    property ColumnType: string read FColumnType write SetColumnType;
    property ModelType: string read FModelType write SetModelType;
    property DefaultVale: string read FDefaultVale write SetDefaultVale;
    property Skip: boolean read FSkip write SetSkip;
  end;

  { TConfigurationTypeHandler }

  TConfigurationTypeHandler = class
  private
    FDatabaseType: string;
    FGetMethod: string;
    FModel: string;
    FSetMethod: string;
    procedure SetDatabaseType(AValue: string);
    procedure SetGetMethod(AValue: string);
    procedure SetModel(AValue: string);
    procedure SetSetMethod(AValue: string);
  public
    class function createNew(dbType, modelType, getter, setter: string): TConfigurationTypeHandler;
    property DatabaseType: string read FDatabaseType write SetDatabaseType;
    property Model: string read FModel write SetModel;
    property GetMethod: string read FGetMethod write SetGetMethod;
    property SetMethod: string read FSetMethod write SetSetMethod;
  end;

  { TConfigurationTable }

  TConfigurationTable = class
  private
    FColumns: TConfigurationColumns;
    FCompilationUnitName: string;
    FForeingKey: TConfigurationTableForeingKeys;
    FImplName: string;
    FIntfName: string;
    FMapperName: string;
    FPrimaryKey: TConfigurationTablePrimaryKey;
    FSkip: boolean;
    FTableName: string;
    FMapper: TConfigurationMapper;
    function getColumns: TConfigurationColumns;
    function GetForeingKey: TConfigurationTableForeingKeys;
    function getMapper: TConfigurationMapper;
    function GetPrimaryKey: TConfigurationTablePrimaryKey;
    procedure SetColumns(AValue: TConfigurationColumns);
    procedure SetCompilationUnitName(AValue: string);
    procedure SetForeingKey(AValue: TConfigurationTableForeingKeys);
    procedure SetImplName(AValue: string);
    procedure SetIntfName(AValue: string);
    procedure SetMapperName(AValue: string);
    procedure SetPrimaryKey(AValue: TConfigurationTablePrimaryKey);
    procedure SetSkip(AValue: boolean);
    procedure SetTableName(AValue: string);
  public
    destructor Destroy; override;
    property TableName: string read FTableName write SetTableName;
    property IntfName: string read FIntfName write SetIntfName;
    property ImplName: string read FImplName write SetImplName;
    property MapperName: string read FMapperName write SetMapperName;
    property Skip: boolean read FSkip write SetSkip;
    property Columns: TConfigurationColumns read getColumns write SetColumns;
    property CompilationUnitName: string read FCompilationUnitName write SetCompilationUnitName;
    function getColumnByName(aColumnName: string): TConfigurationColumn;
    property MapperConfiguration: TConfigurationMapper read getMapper;
    property PrimaryKey: TConfigurationTablePrimaryKey read GetPrimaryKey;
    property ForeingKey: TConfigurationTableForeingKeys read GetForeingKey;
  end;

  { TConfigurationParameter }

  TConfigurationParameter = class
  private
    FParameterName: string;
    FParameterType: string;
    procedure SetParameterName(AValue: string);
    procedure SetParameterType(AValue: string);
  public
    property ParameterName: string read FParameterName write SetParameterName;
    property ParameterType: string read FParameterType write SetParameterType;
  end;

  { TConfigurationMapperMethod }

  TConfigurationMapperMethod = class
  private
    FBodyMethod: string;
    FMethodName: string;
    FParameters: TConfigurationParameters;
    FResultName: string;
    function GetParameters: TConfigurationParameters;
    procedure SetBodyMethod(AValue: string);
    procedure SetMethodName(AValue: string);
    procedure SetResultName(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property MethodName: string read FMethodName write SetMethodName;
    property Parameters: TConfigurationParameters read GetParameters;
    property BodyMethod: string read FBodyMethod write SetBodyMethod;
    property ResultName: string read FResultName write SetResultName;
  end;

  TConfigurationMapper = class
  private
    FGlobalName: string;
    function getMethods: TConfigurationMapperMethods;
    procedure SetGlobalName(AValue: string);
  protected
    FMethods: TConfigurationMapperMethods;
  public
    destructor Destroy; override;
    property methods: TConfigurationMapperMethods read getMethods;
    property GlobalName: string read FGlobalName write SetGlobalName;
  end;

  { TConfigurationConnection }

  TConfigurationConnection = class
  private
    FPassword: string;
    FUrl: string;
    FUserName: string;
    procedure SetPassword(AValue: string);
    procedure SetUrl(AValue: string);
    procedure SetUserName(AValue: string);
  public
    property Url: string read FUrl write SetUrl;
    property UserName: string read FUserName write SetUserName;
    property Password: string read FPassword write SetPassword;
  end;

  { TConfigurationContext }

  TConfigurationContext = class(TComponent)
  private
    FCompilationUnits: TOMCompilationUnits;
    FCopyRight: string;
    FNamedItems: TOMNamedItems;
    FId: string;
    FTables: TConfigurationTables;
    FtargetLocation: string;
    FTypes: TConfigurationTypeHandlers;
    FZdbcConnection: TConfigurationConnection;
    function GetCompilationUnits: TOMCompilationUnits;
    function GetTables: TConfigurationTables;
    function GetTypes: TConfigurationTypeHandlers;
    procedure SetCopyRight(AValue: string);
    procedure SetId(AValue: string);
    procedure SetTables(AValue: TConfigurationTables);
    procedure SetTargetLocation(AValue: string);
    procedure SetZdbcConnection(AValue: TConfigurationConnection);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function getCompilationUnit(aCompilationUnitName: string): TOMCompilationUnit;
    function getNamedItem(aName: string): TOMNamedItem;
    procedure addNamedItem(aName: string);
    procedure addNamedItem(aName: TOMNamedItem);
    function getTable(aTableName: string): TConfigurationTable;
    property CompilationUnits: TOMCompilationUnits read GetCompilationUnits;
    property Id: string read FId write SetId;
    property Tables: TConfigurationTables read GetTables write SetTables;
    property targetLocation: string read FtargetLocation write SettargetLocation;
    property zdbcConnection: TConfigurationConnection read FzdbcConnection write SetzdbcConnection;
    property Types: TConfigurationTypeHandlers read GetTypes;
    function findTypeHandler(columnType: string): TConfigurationTypeHandler;
    function findTypeGetter(modelType: string): TConfigurationTypeHandler;
    property CopyRight: string read FCopyRight write SetCopyRight;
  end;



  TConfiguration = class(TComponent)
  private
    FContextes: TConfigurationContextes;
    function getContextes: TConfigurationContextes;
    procedure SetContextes(AValue: TConfigurationContextes);
  public
    property Contextes: TConfigurationContextes read getContextes write SetContextes;
  end;

implementation

{ TConfigurationTableForeingKey }

function TConfigurationTableForeingKey.GetColumns: TConfigurationColumns;
begin
  if FColumns = nil then
  begin
    FColumns := TConfigurationColumns.Create(False);
  end;
  Result := FColumns;
end;

destructor TConfigurationTableForeingKey.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

{ TConfigurationTablePrimaryKey }

function TConfigurationTablePrimaryKey.GetColumns: TConfigurationColumns;
begin
  if FColumns = nil then
  begin
    FColumns := TConfigurationColumns.Create(False);
  end;
  Result := FColumns;
end;

destructor TConfigurationTablePrimaryKey.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

{ TConfigurationMapper }

function TConfigurationMapper.getMethods: TConfigurationMapperMethods;
begin
  if FMethods = nil then
  begin
    FMethods := TConfigurationMapperMethods.Create(True);
  end;
  Result := FMethods;
end;

procedure TConfigurationMapper.SetGlobalName(AValue: string);
begin
  if FGlobalName = AValue then
  begin
    Exit;
  end;
  FGlobalName := AValue;
end;

destructor TConfigurationMapper.Destroy;
begin
  FreeAndNil(FMethods);
  inherited Destroy;
end;

{ TConfigurationParameter }

procedure TConfigurationParameter.SetParameterName(AValue: string);
begin
  if FParameterName = AValue then
  begin
    Exit;
  end;
  FParameterName := AValue;
end;

procedure TConfigurationParameter.SetParameterType(AValue: string);
begin
  if FParameterType = AValue then
  begin
    Exit;
  end;
  FParameterType := AValue;
end;

{ TConfigurationMapperMethod }

function TConfigurationMapperMethod.GetParameters: TConfigurationParameters;
begin
  if FParameters = nil then
  begin
    FParameters := TConfigurationParameters.Create(True);
  end;
  Result := FParameters;
end;

procedure TConfigurationMapperMethod.SetBodyMethod(AValue: string);
begin
  if FBodyMethod = AValue then
  begin
    Exit;
  end;
  FBodyMethod := AValue;
end;

procedure TConfigurationMapperMethod.SetMethodName(AValue: string);
begin
  if FMethodName = AValue then
  begin
    Exit;
  end;
  FMethodName := AValue;
end;

procedure TConfigurationMapperMethod.SetResultName(AValue: string);
begin
  if FResultName = AValue then
  begin
    Exit;
  end;
  FResultName := AValue;
end;

constructor TConfigurationMapperMethod.Create;
begin

end;

destructor TConfigurationMapperMethod.Destroy;
begin
  FreeAndNil(FParameters);
  inherited Destroy;
end;

{ TConfigurationTypeHandler }

procedure TConfigurationTypeHandler.SetDatabaseType(AValue: string);
begin
  if FDatabaseType = AValue then
  begin
    Exit;
  end;
  FDatabaseType := AValue;
end;

procedure TConfigurationTypeHandler.SetGetMethod(AValue: string);
begin
  if FGetMethod = AValue then
  begin
    Exit;
  end;
  FGetMethod := AValue;
end;

procedure TConfigurationTypeHandler.SetModel(AValue: string);
begin
  if FModel = AValue then
  begin
    Exit;
  end;
  FModel := AValue;
end;

procedure TConfigurationTypeHandler.SetSetMethod(AValue: string);
begin
  if FSetMethod = AValue then
  begin
    Exit;
  end;
  FSetMethod := AValue;
end;

class function TConfigurationTypeHandler.createNew(dbType, modelType, getter, setter: string): TConfigurationTypeHandler;

begin
  Result := TConfigurationTypeHandler.Create;
  Result.DatabaseType := dbType;
  Result.Model := modelType;
  Result.GetMethod := getter;
  Result.SetMethod := setter;
end;

{ TConfigurationTable }

procedure TConfigurationTable.SetColumns(AValue: TConfigurationColumns);
begin
  if FColumns = AValue then
  begin
    Exit;
  end;
  FColumns := AValue;
end;

procedure TConfigurationTable.SetCompilationUnitName(AValue: string);
begin
  if FCompilationUnitName = AValue then
  begin
    Exit;
  end;
  FCompilationUnitName := AValue;
end;

procedure TConfigurationTable.SetForeingKey(AValue: TConfigurationTableForeingKeys);
begin
  if FForeingKey = AValue then
  begin
    Exit;
  end;
  FForeingKey := AValue;
end;

function TConfigurationTable.getColumns: TConfigurationColumns;
begin
  if FColumns = nil then
  begin
    FColumns := TConfigurationColumns.Create(True);
  end;
  Result := FColumns;
end;

function TConfigurationTable.GetForeingKey: TConfigurationTableForeingKeys;
begin
  if FForeingKey = nil then
  begin
    FForeingKey := TConfigurationTableForeingKeys.Create(True);
  end;
  Result := FForeingKey;
end;

function TConfigurationTable.getMapper: TConfigurationMapper;
begin
  if FMapper = nil then
  begin
    FMapper := TConfigurationMapper.Create;
  end;
  Result := FMapper;
end;

function TConfigurationTable.GetPrimaryKey: TConfigurationTablePrimaryKey;
begin
  if FPrimaryKey = nil then
  begin
    FPrimaryKey := TConfigurationTablePrimaryKey.Create;
  end;
  Result := FPrimaryKey;
end;

procedure TConfigurationTable.SetImplName(AValue: string);
begin
  if FImplName = AValue then
  begin
    Exit;
  end;
  FImplName := AValue;
end;

procedure TConfigurationTable.SetIntfName(AValue: string);
begin
  if FIntfName = AValue then
  begin
    Exit;
  end;
  FIntfName := AValue;
end;

procedure TConfigurationTable.SetMapperName(AValue: string);
begin
  if FMapperName = AValue then
  begin
    Exit;
  end;
  FMapperName := AValue;
end;

procedure TConfigurationTable.SetPrimaryKey(AValue: TConfigurationTablePrimaryKey);
begin
  if FPrimaryKey = AValue then
  begin
    Exit;
  end;
  FPrimaryKey := AValue;
end;

procedure TConfigurationTable.SetSkip(AValue: boolean);
begin
  if FSkip = AValue then
  begin
    Exit;
  end;
  FSkip := AValue;
end;

procedure TConfigurationTable.SetTableName(AValue: string);
begin
  if FTableName = AValue then
  begin
    Exit;
  end;
  FTableName := AValue;
end;

destructor TConfigurationTable.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FForeingKey);
  FreeAndNil(FPrimaryKey);
  FreeAndNil(FMapper);
  inherited Destroy;
end;

function TConfigurationTable.getColumnByName(aColumnName: string): TConfigurationColumn;
var
  cursor: TConfigurationColumn;
begin
  Result := nil;
  for cursor in Columns do
  begin
    if UpperCase(cursor.ColumnName) = UpperCase(aColumnName) then
    begin
      exit(cursor);
    end;
  end;
end;

{ TConfigurationColumn }

procedure TConfigurationColumn.SetColumnName(AValue: string);
begin
  if FColumnName = AValue then
  begin
    Exit;
  end;
  FColumnName := AValue;
end;

procedure TConfigurationColumn.SetColumnType(AValue: string);
begin
  if FColumnType = AValue then
  begin
    Exit;
  end;
  FColumnType := AValue;
end;

procedure TConfigurationColumn.SetDefaultVale(AValue: string);
begin
  if FDefaultVale = AValue then
  begin
    Exit;
  end;
  FDefaultVale := AValue;
end;

procedure TConfigurationColumn.SetModelName(AValue: string);
begin
  if FModelName = AValue then
  begin
    Exit;
  end;
  FModelName := AValue;
end;

procedure TConfigurationColumn.SetModelType(AValue: string);
begin
  if FModelType = AValue then
  begin
    Exit;
  end;
  FModelType := AValue;
end;

procedure TConfigurationColumn.SetSkip(AValue: boolean);
begin
  if FSkip = AValue then
  begin
    Exit;
  end;
  FSkip := AValue;
end;

{ TConfiguration }

procedure TConfiguration.SetContextes(AValue: TConfigurationContextes);
begin
  if FContextes = AValue then
  begin
    Exit;
  end;
  FContextes := AValue;
end;

function TConfiguration.getContextes: TConfigurationContextes;
begin
  if FContextes = nil then
  begin
    FContextes := TConfigurationContextes.Create(True);
  end;
  Result := FContextes;
end;

{ TConfigurationContext }

function TConfigurationContext.GetTables: TConfigurationTables;
begin
  if FTables = nil then
  begin
    FTables := TConfigurationTables.Create(True);
  end;
  Result := FTables;
end;

function TConfigurationContext.GetTypes: TConfigurationTypeHandlers;
begin
  if FTypes = nil then
  begin
    FTypes := TConfigurationTypeHandlers.Create(True);
  end;
  Result := FTypes;
end;

procedure TConfigurationContext.SetCopyRight(AValue: string);
begin
  if FCopyRight = AValue then
  begin
    Exit;
  end;
  FCopyRight := AValue;
end;

function TConfigurationContext.GetCompilationUnits: TOMCompilationUnits;
begin
  if FCompilationUnits = nil then
  begin
    FCompilationUnits := TOMCompilationUnits.Create(True);
  end;
  Result := FCompilationUnits;
end;

procedure TConfigurationContext.SetId(AValue: string);
begin
  if FId = AValue then
  begin
    Exit;
  end;
  FId := AValue;
end;

procedure TConfigurationContext.SetTables(AValue: TConfigurationTables);
begin
  if FTables = AValue then
  begin
    Exit;
  end;
  FTables := AValue;
end;

procedure TConfigurationContext.SetTargetLocation(AValue: string);
begin
  if FtargetLocation = AValue then
  begin
    Exit;
  end;
  FtargetLocation := AValue;
end;

procedure TConfigurationContext.SetZdbcConnection(AValue: TConfigurationConnection);
begin
  if FzdbcConnection = AValue then
  begin
    Exit;
  end;
  FzdbcConnection := AValue;
end;

constructor TConfigurationContext.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FZdbcConnection := nil;
  FNamedItems     := TOMNamedItems.Create(False);
end;

destructor TConfigurationContext.Destroy;
begin
  FreeAndNil(FzdbcConnection);
  FreeAndNil(FTables);
  FreeAndNil(FCompilationUnits);
  FreeAndNil(FNamedItems);
  FreeAndNil(FTypes);
  inherited Destroy;
end;

function TConfigurationContext.getTable(aTableName: string): TConfigurationTable;
var
  cursor: TConfigurationTable;
begin
  Result := nil;
  for cursor in Tables do
  begin
    if cursor.TableName = aTableName then
    begin
      exit(cursor);
    end;
  end;
end;

function TConfigurationContext.findTypeHandler(columnType: string): TConfigurationTypeHandler;
var
  cursor: TConfigurationTypeHandler;
begin
  for cursor in Types do
  begin
    if UpperCase(cursor.DatabaseType) = UpperCase(columnType) then
    begin
      exit(cursor);
    end;
  end;
  Result := TConfigurationTypeHandler.createNew(columnType, columnType, 'get' + columnType, 'set' + columnType);
  Types.Add(Result);
end;

function TConfigurationContext.findTypeGetter(modelType: string): TConfigurationTypeHandler;
var
  cursor: TConfigurationTypeHandler;
begin
  Result := nil;
  for cursor in Types do
  begin
    if UpperCase(cursor.Model) = UpperCase(modelType) then
    begin
      exit(cursor);
    end;
  end;
end;

function TConfigurationContext.getCompilationUnit(aCompilationUnitName: string): TOMCompilationUnit;
var
  cursor: TOMCompilationUnit;
begin
  Result := nil;
  for cursor in CompilationUnits do
  begin
    if cursor.UnitName = aCompilationUnitName then
    begin
      exit(cursor);
    end;
  end;
end;

function TConfigurationContext.getNamedItem(aName: string): TOMNamedItem;
var
  item: TOMNamedItem;
begin
  for item in FNamedItems do
  begin
    if UpperCase(item.Name) = UpperCase(aName) then
    begin
      exit(item);
    end;
  end;
  Result := TOMNamedItem.Create(aName);
  FNamedItems.Add(Result);
end;

procedure TConfigurationContext.addNamedItem(aName: string);
begin
  FNamedItems.Add(TOMNamedItem.Create(aName));
end;

procedure TConfigurationContext.addNamedItem(aName: TOMNamedItem);
begin
  FNamedItems.Add(aName);
end;

{ TConfigurationConnection }

procedure TConfigurationConnection.SetPassword(AValue: string);
begin
  if FPassword = AValue then
  begin
    Exit;
  end;
  FPassword := AValue;
end;

procedure TConfigurationConnection.SetUrl(AValue: string);
begin
  if FUrl = AValue then
  begin
    Exit;
  end;
  FUrl := AValue;
end;

procedure TConfigurationConnection.SetUserName(AValue: string);
begin
  if FUserName = AValue then
  begin
    Exit;
  end;
  FUserName := AValue;
end;

end.
