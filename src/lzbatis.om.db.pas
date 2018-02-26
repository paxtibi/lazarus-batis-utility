unit lzbatis.om.DB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ZDbcIntfs;

type
  TOMTable = class;
  TOMColumn = class;

  TOMTables = specialize TFPGObjectList<TOMTable>;
  TOMColumns = specialize TFPGObjectList<TOMColumn>;

  { TOMColumn }
  TNullableColumn = (
    columnNoNulls,// - might not allow NULL values
    columnNullable,//- definitely allows NULL values
    columnNullableUnknown//- nullability unknown
    );

  TOMColumn = class
  private
    FColumnDefault: string;
    FColumnKey: boolean;
    FColumnName: string;
    FColumnNullable: TNullableColumn;
    FColumnPrecision: integer;
    FColumnSize: integer;
    FColumnType: TZSQLType;
    FColumnTypeName: string;
    FColumnTypeValue: integer;
    procedure SetColumnDefault(AValue: string);
    procedure SetColumnKey(AValue: boolean);
    procedure SetColumnName(AValue: string);
    procedure SetColumnNullable(AValue: TNullableColumn);
    procedure SetColumnPrecision(AValue: integer);
    procedure SetColumnSize(AValue: integer);
    procedure SetColumnType(AValue: TZSQLType);
    procedure SetColumnTypeName(AValue: string);
    procedure SetColumnTypeValue(AValue: integer);
  published
    property ColumnName: string read FColumnName write SetColumnName;
    property ColumnTypeName: string read FColumnTypeName write SetColumnTypeName;
    property ColumnTypeValue: integer read FColumnTypeValue write SetColumnTypeValue;
    property ColumnType: TZSQLType read FColumnType write SetColumnType;
    property ColumnSize: integer read FColumnSize write SetColumnSize;
    property ColumnPrecision: integer read FColumnPrecision write SetColumnPrecision;
    property ColumnNullable: TNullableColumn read FColumnNullable write SetColumnNullable;
    property ColumnDefault: string read FColumnDefault write SetColumnDefault;
    property ColumnKey: boolean read FColumnKey write SetColumnKey;
  end;

  { TOMTable }

  TOMTable = class
  private
    FTableColumns: TOMColumns;
    FTableName: string;
    procedure SetTableColumns(AValue: TOMColumns);
    procedure SetTableName(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property tableName: string read FTableName write SetTableName;
    property tableColumns: TOMColumns read FTableColumns write SetTableColumns;
  end;

  { TOMDatabase }

  TOMDatabase = class
  private
    FTables: TOMTables;
    procedure SetTables(AValue: TOMTables);
  public
    constructor Create;
    destructor Destroy; override;
    function getTable(tableName: string): TOMTable;
  published
    property tables: TOMTables read FTables write Settables;
  end;

implementation

{ TOMDatabase }

procedure TOMDatabase.SetTables(AValue: TOMTables);
begin
  if FTables = AValue then
  begin
    Exit;
  end;
  FTables := AValue;
end;

constructor TOMDatabase.Create;
begin
  FTables := nil;
end;

destructor TOMDatabase.Destroy;
begin
  FreeAndNil(FTables);
  inherited Destroy;
end;

function TOMDatabase.getTable(tableName: string): TOMTable;
var
  cursor: TOMTable;
begin
  Result := nil;
  for cursor in FTables do
  begin
    if cursor.tableName = tableName then
    begin
      exit(cursor);
    end;
  end;
end;

{ TOMColumn }

procedure TOMColumn.SetColumnName(AValue: string);
begin
  if FColumnName = AValue then
  begin
    Exit;
  end;
  FColumnName := AValue;
end;

procedure TOMColumn.SetColumnDefault(AValue: string);
begin
  if FColumnDefault = AValue then
  begin
    Exit;
  end;
  FColumnDefault := AValue;
end;

procedure TOMColumn.SetColumnKey(AValue: boolean);
begin
  if FColumnKey = AValue then
  begin
    Exit;
  end;
  FColumnKey := AValue;
end;

procedure TOMColumn.SetColumnNullable(AValue: TNullableColumn);
begin
  if FColumnNullable = AValue then
  begin
    Exit;
  end;
  FColumnNullable := AValue;
end;

procedure TOMColumn.SetColumnPrecision(AValue: integer);
begin
  if FColumnPrecision = AValue then
  begin
    Exit;
  end;
  FColumnPrecision := AValue;
end;

procedure TOMColumn.SetColumnSize(AValue: integer);
begin
  if FColumnSize = AValue then
  begin
    Exit;
  end;
  FColumnSize := AValue;
end;

procedure TOMColumn.SetColumnType(AValue: TZSQLType);
begin
  if FColumnType = AValue then
  begin
    Exit;
  end;
  FColumnType := AValue;
end;

procedure TOMColumn.SetColumnTypeName(AValue: string);
begin
  if FColumnTypeName = AValue then
  begin
    Exit;
  end;
  FColumnTypeName := AValue;
end;

procedure TOMColumn.SetColumnTypeValue(AValue: integer);
begin
  if FColumnTypeValue = AValue then
  begin
    Exit;
  end;
  FColumnTypeValue := AValue;
end;

{ TOMTable }

procedure TOMTable.SetTableColumns(AValue: TOMColumns);
begin
  if FTableColumns = AValue then
  begin
    Exit;
  end;
  FTableColumns := AValue;
end;

procedure TOMTable.SetTableName(AValue: string);
begin
  if FTableName = AValue then
  begin
    Exit;
  end;
  FTableName := AValue;
end;

constructor TOMTable.Create;
begin
  FTableColumns := nil;
end;

destructor TOMTable.Destroy;
begin
  FreeAndNil(FTableColumns);
  inherited Destroy;
end;

end.
