unit lzBatis.application;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, CustApp, ZDbcIntfs, lzBatis.om.config, lzBatis.om.pascal;

type
  { TlzBatisGenerator }
  TlzBatisGenerator = class(TCustomApplication)
  private
    // Configurazione XML
    function leggiColonna(node: TDOMNode): TConfigurationColumn;
    procedure leggiConfigurazioneMapper(node: TDOMNode; mapperConfig: TConfigurationMapper);
    function leggiConnessione(node: TDOMNode): TConfigurationConnection;
    procedure leggiContesto(node: TDOMNode);
    function leggiMapperMethod(node: TDOMNode): TConfigurationMapperMethod;
    function leggiTabella(node: TDOMNode): TConfigurationTable;
    function generaSelectSQLPerPrimaryKey(tabella: TConfigurationTable): string;
    function generaUpdateSQLPerPrimaryKey(tabella: TConfigurationTable): string;
    function generaDeleteSQLPerPrimaryKey(tabella: TConfigurationTable): string;
    function generaInsertSQLPerPrimaryKey(tabella: TConfigurationTable): string;
  protected
    FCompliationUnits: TOMCompilationUnits;
    FTargetLocation: string;
    FSkipPattern: string;
    FConnection: IZConnection;
    FTables: TStringList;
    FOverride: boolean;
    FFileNameInput: string;
    FConfiguration: TConfiguration;
    procedure DbgCursor(const resultSet: IZResultSet);
    procedure completaConfigurazione(const aContext: TConfigurationContext);
    procedure processaColonne(const aContext: TConfigurationContext; const aTable: TConfigurationTable; target: TOMAggregateItem);
    procedure attivaConnessione(const aContext: TConfigurationContext);
    procedure prepareCompilationUnit(const aContext: TConfigurationContext);
    procedure prepareInterfaces(const aContext: TConfigurationContext);
    procedure prepareEntities(const aContext: TConfigurationContext);
    procedure prepareMappers(const aContext: TConfigurationContext);
    procedure processaContesto(const aContext: TConfigurationContext);
    procedure scriviContesto(const aContext: TConfigurationContext);
    procedure disattivaConnessione;
    procedure leggiConfigurazione(document: TXMLDocument);
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  lzBatiz.writers,
  RegExpr, Laz2_XMLRead, lzBatis.dom.aspects, ZDbcInterbase6, ZDbcAdo, ZDbcDbLib,
  ZDbcMySql, ZDbcOracle, ZDbcPostgreSql, ZDbcSqLite,
  LazLogger, fileutil;

const
  SQLTypeName: array [TZSQLType] of string = ('Unknown', 'Boolean',
    'Byte', 'Short', 'Word', 'Small', 'LongWord', 'Integer', 'ULong', 'Long',
    'Float', 'Double', 'Currency', 'BigDecimal',
    'String', 'UnicodeString',
    'Bytes', 'GUID',
    'Date', 'Time', 'Timestamp',
    'Array', 'DataSet',
    'AsciiStream', 'UnicodeStream', 'BinaryStream');

function CapCase(const s: string): string;
begin
  Result := s;
  if s <> '' then
  begin
    Result[1] := upcase(Result[1]);
  end;
end;

{ lzBatisGenerator }

procedure TlzBatisGenerator.DbgCursor(const resultSet: IZResultSet);
var
  rsmd: IZResultSetMetadata;
  idx: integer;
begin
  rsmd := resultSet.GetMetadata;
  for idx := 1 to rsmd.GetColumnCount do
  begin
    DebugLn(rsmd.GetColumnName(idx), ':', rsmd.GetColumnTypeName(idx), '(', rsmd.GetColumnLabel(idx), ')');
  end;
end;

procedure TlzBatisGenerator.completaConfigurazione(const aContext: TConfigurationContext);
var
  tableName: string;
  table: TConfigurationTable;
  column: TConfigurationColumn;
  tableResultSet: IZResultSet;
  columnResultSet: IZResultSet;
  tableTypes: array of string;
begin
  DebugLnEnter('completaConfigurazione');
  SetLength(tableTypes, 1);
  tableTypes[0] := 'TABLE';
  aContext.Types.Add(TConfigurationTypeHandler.createNew('BigDecimal', 'Extended', 'GetExtended', 'SetExtended'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Boolean', 'Boolean', 'GetBoolean', 'SetBoolean'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Currency', 'Currency', 'GetCurrency', 'SetCurrency'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Double', 'Double', 'GetDouble', 'SetDouble'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Float', 'Single', 'GetFloat', 'SetFloat'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Integer', 'Int32', 'GetInteger', 'SetInteger'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Short', 'Shortint', 'GetShort', 'SetShort'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Long', 'Int64', 'GetLong', 'SetLong'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Int64', 'Int64', 'GetLong', 'SetLong'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Int32', 'Int32', 'GetInt', 'GetInt'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Small', 'Smallint', 'GetSmall', 'SetSmall'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Smallint', 'Smallint', 'GetSmall', 'SetSmall'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Timestamp', 'TDateTime', 'GetTimestamp', 'SetTimestamp'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('TDateTime', 'TDateTime', 'GetTimestamp', 'SetTimestamp'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Time', 'TDateTime', 'GetTime', 'SetTime'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('Date', 'TDateTime', 'GetDate', 'SetDate'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('AsciiStream', 'String', 'GetString', 'SetString'));
  aContext.Types.Add(TConfigurationTypeHandler.createNew('String', 'String', 'GetString', 'SetString'));
  tableResultSet := FConnection.GetMetadata.GetTables('', '', '%', tableTypes);
  while tableResultSet.Next do
  begin
    if tableResultSet.GetStringByName('TABLE_TYPE') = 'TABLE' then
    begin
      tableName := tableResultSet.GetStringByName('TABLE_NAME');
      table     := aContext.getTable(tableName);
      if table = nil then
      begin
        table := TConfigurationTable.Create;
        table.TableName := tableName;
        aContext.Tables.Add(table);
        table.ImplName   := 'T' + CapCase(tableName);
        table.IntfName   := 'I' + CapCase(tableName);
        table.MapperName := 'T' + CapCase(tableName) + 'Mapper';
        table.CompilationUnitName := tableName;
      end;
      if table <> nil then
      begin
        columnResultSet := FConnection.GetMetadata.GetColumns('', '', table.TableName, '%');
        //DbgCursor(columnResultSet);
        while columnResultSet.Next do
        begin
          column := table.getColumnByName(columnResultSet.GetStringByName('COLUMN_NAME'));
          if column = nil then
          begin
            column := TConfigurationColumn.Create;
            table.Columns.Add(column);
          end;
          column.ModelType   := aContext.findTypeHandler(SQLTypeName[TZSQLType(columnResultSet.GetIntByName('DATA_TYPE'))]).Model;
          column.ColumnType  := columnResultSet.GetStringByName('TYPE_NAME');
          column.ModelName   := aContext.findTypeHandler(columnResultSet.GetStringByName('COLUMN_NAME')).Model;
          column.ColumnName  := columnResultSet.GetStringByName('COLUMN_NAME');
          column.DefaultVale := columnResultSet.GetStringByName('COLUMN_DEF');
          DebugLn(tableName, ':', column.ColumnName, ' -> ', column.ModelName, '(', column.ColumnType, ' -> ', column.ModelType, ') <-', column.DefaultVale);
        end;
      end;
      columnResultSet := FConnection.GetMetadata.GetPrimaryKeys('', '', tableName);
      while columnResultSet.Next do
      begin
        table.PrimaryKey.Columns.Add(table.getColumnByName(columnResultSet.GetStringByName('COLUMN_NAME')));
      end;
      DebugLnEnter(table.TableName, ' key columns count ', table.PrimaryKey.Columns.Count.ToString);
      for column in table.PrimaryKey.Columns do
      begin
        DebugLn(column.ColumnName);
      end;
      DebugLnExit('key columns');
    end;
  end;
  DebugLnExit('completaConfigurazione');
end;

procedure TlzBatisGenerator.processaColonne(const aContext: TConfigurationContext; const aTable: TConfigurationTable; target: TOMAggregateItem);
var
  cursor: TConfigurationColumn;
  modelGet: TOMMethod;
  modelSet: TOMMethod;
  field: TOMField;
  parameter: TOMParameter;
  _property: TOMProperty;
begin
  for cursor in aTable.Columns do
  begin
    modelGet := TOMMethod.Create('get' + cursor.ModelName);
    modelGet.ReturnType := aContext.getNamedItem(cursor.ModelType);
    modelSet := TOMMethod.Create('set' + cursor.ModelName);
    modelSet.ReturnType := nil;

    field := TOMField.Create('F' + cursor.ModelName);
    field.ReferencedColumn := nil;
    field.ReferencedType := aContext.getNamedItem(cursor.ModelType);

    parameter := TOMParameter.Create('aValue');
    parameter.ParameterType := aContext.getNamedItem(cursor.ModelType);
    parameter.ParameterProtocolo := ppConst;
    modelSet.Parameters.Add(parameter);

    target.Fields.Add(field);
    target.Methods.Add(modelGet);
    target.Methods.Add(modelSet);
    modelSet.SetterOf := field;
    modelGet.SetterOf := field;

    field.Visibility    := vlProtected;
    modelGet.Visibility := vlPublic;
    modelSet.Visibility := vlPublic;
    if target is TOMInterface then
    begin
      _property := TOMProperty.Create(cursor.ModelName);
      _property.Setter := modelSet;
      _property.Getter := modelGet;
      (target as TOMInterface).Properties.Add(_property);
    end;
  end;
end;

procedure TlzBatisGenerator.attivaConnessione(const aContext: TConfigurationContext);
begin
  FConnection := DriverManager.GetConnectionWithLogin(aContext.zdbcConnection.Url, aContext.zdbcConnection.UserName, aContext.zdbcConnection.Password);
end;

procedure TlzBatisGenerator.prepareCompilationUnit(const aContext: TConfigurationContext);
var
  cursor: TConfigurationTable;
  cu: TOMCompilationUnit;
  Count: integer = 1;
  renamedFile: string;
begin
  DebugLnEnter('prepareCompilationUnit');
  for cursor in aContext.Tables do
  begin
    DebugLn(cursor.CompilationUnitName);
    cu := aContext.getCompilationUnit(cursor.CompilationUnitName);
    DebugLn(cursor.CompilationUnitName);
    if cu = nil then
    begin
      cu := TOMCompilationUnit.Create;
      cu.Copyright := aContext.CopyRight;
      cu.UnitName := cursor.CompilationUnitName;
      cu.FileName := ExcludeTrailingBackslash(aContext.targetLocation) + '/' + cursor.CompilationUnitName + '.pas';
      cu.PublicDependences.Add(TOMNamedItem.Create('Classes'));
      cu.PublicDependences.Add(TOMNamedItem.Create('LzBatis.lib'));
      cu.PrivateDependences.Add(TOMNamedItem.Create('SysUtils'));

      aContext.CompilationUnits.Add(cu);

      DebugLn(cu.FileName);
      if FileExists(cu.FileName) then
      begin
        if FOverride then
        begin
          DeleteFile(cu.FileName);
        end
        else
        begin
          Count := 1;
          repeat
            renamedFile := Format('%s.%d', [cu.FileName, Count]);
            Inc(Count);
          until not FileExists(renamedFile);
          RenameFile(cu.FileName, renamedFile);
        end;
      end;
    end;
  end;
  DebugLnExit('prepareCompilationUnit');
end;

procedure TlzBatisGenerator.prepareInterfaces(const aContext: TConfigurationContext);
var
  cursor: TConfigurationTable;
  cu: TOMCompilationUnit;
  intf: TOMInterface;
begin
  DebugLnEnter('prepareInterfaces');
  DebugLn(aContext.Id);
  for cursor in aContext.Tables do
  begin
    cu := aContext.getCompilationUnit(cursor.CompilationUnitName);
    if cu <> nil then
    begin
      intf := cu.getInterfaceByName(cursor.IntfName);
      if intf = nil then
      begin
        intf := TOMInterface.Create(cursor.IntfName);
        intf.ConcreteClass := cu.getClassByName(cursor.ImplName);
        if intf.ConcreteClass = nil then
        begin
          intf.ConcreteClass := TOMClass(aContext.getNamedItem(cursor.ImplName));
        end;
        if intf.ConcreteClass <> nil then
        begin
          TOMClass(intf.ConcreteClass).Impls.Add(intf);
        end;
        cu.Interfaces.Add(intf);
        processaColonne(aContext, cursor, intf);
        aContext.addNamedItem(intf);
      end;
    end
    else
    begin
      DebugLn('prepareInterfaces:', cursor.CompilationUnitName, ' not found!');
    end;
  end;
  DebugLnExit('prepareInterfaces');
end;

procedure TlzBatisGenerator.prepareEntities(const aContext: TConfigurationContext);
var
  cursor: TConfigurationTable;
  cu: TOMCompilationUnit;
  impl: TOMClass;
begin
  DebugLnEnter('prepareEntities');
  DebugLn(aContext.Id);
  for cursor in aContext.Tables do
  begin
    cu := aContext.getCompilationUnit(cursor.CompilationUnitName);
    if cu <> nil then
    begin
      impl := cu.getClassByName(cursor.ImplName);
      if impl = nil then
      begin
        impl := TOMClass.Create(cursor.ImplName);
        cu.Classes.Add(impl);
        aContext.addNamedItem(impl);
      end;
      processaColonne(aContext, cursor, impl);
    end
    else
    begin
      DebugLn('prepareEntities:', cursor.CompilationUnitName, ' not found!');
    end;
  end;
  DebugLnExit('prepareEntities');
end;

procedure TlzBatisGenerator.prepareMappers(const aContext: TConfigurationContext);
var
  cursor: TConfigurationTable;
  confColumn: TConfigurationColumn;
  cu: TOMCompilationUnit;
  mapper: TOMMapper;
  mapperMethod: TOMMethod;
  mapperField: TOMField;
  mapperParameter: TOMParameter;
  configMethod: TConfigurationMapperMethod;
  configParameter: TConfigurationParameter;
begin
  DebugLnEnter('prepareMappers');
  DebugLn(aContext.Id);
  for cursor in aContext.Tables do
  begin
    cu := aContext.getCompilationUnit(cursor.CompilationUnitName);
    if cu <> nil then
    begin
      mapper := cu.getMapperByName(cursor.MapperName);
      if mapper = nil then
      begin
        mapper := TOMMapper.Create(cursor.MapperName);
        cu.Mappers.Add(mapper);
        aContext.addNamedItem(mapper);
        for configMethod in cursor.MapperConfiguration.methods do
        begin
          mapperField  := TOMField.Create('F' + CapCase(configMethod.MethodName));
          mapperField.ReferencedType := aContext.getNamedItem('IZPreparedStatement');
          mapperField.TypeName := 'IZPreparedStatement';
          mapperField.Visibility := vlProtected;
          mapperField.InitializiationValue := configMethod.BodyMethod;
          mapperMethod := TOMMethod.Create(configMethod.MethodName);
          mapperMethod.Visibility := vlPublic;
          mapperMethod.Body := configMethod.BodyMethod;
          mapperMethod.ResultName := configMethod.ResultName;
          mapperMethod.SetterOf := mapperField;
          for configParameter in configMethod.Parameters do
          begin
            mapperParameter := TOMParameter.Create(configParameter.ParameterName);
            mapperParameter.ParameterTypeName := configParameter.ParameterType;
            mapperParameter.ParameterProtocolo := ppConst;

            mapperMethod.Parameters.Add(mapperParameter);
          end;
          mapper.Methods.Add(mapperMethod);
          mapper.Fields.Add(mapperField);
        end;

        if cursor.PrimaryKey.Columns.Count > 0 then
        begin
          mapperMethod := TOMMethod.Create('get' + CapCase(cursor.IntfName) + 'ByKey');
          mapperField  := TOMField.Create('FSelect' + CapCase(cursor.IntfName) + 'ByKey');

          mapper.Methods.Add(mapperMethod);
          mapper.Fields.Add(mapperField);

          mapperField.ReferencedType := aContext.getNamedItem('IZPreparedStatement');
          mapperField.TypeName    := 'IZPreparedStatement';
          mapperField.Visibility  := vlProtected;
          mapperField.InitializiationValue := generaSelectSQLPerPrimaryKey(cursor);
          mapperMethod.Visibility := vlPublic;
          mapperMethod.SetterOf   := mapperField;
          mapperMethod.ReturnType := aContext.getNamedItem(cursor.IntfName);
          for confColumn in cursor.PrimaryKey.Columns do
          begin
            mapperParameter := TOMParameter.Create('a' + CapCase(confColumn.ModelName));
            mapperParameter.ParameterTypeName := confColumn.ModelType;
            mapperParameter.ParameterProtocolo := ppConst;
            mapperMethod.Parameters.Add(mapperParameter);
          end;
        end;

        if cursor.PrimaryKey.Columns.Count > 0 then
        begin
          mapperMethod := TOMMethod.Create('save' + CapCase(cursor.IntfName));
          mapperField  := TOMField.Create('FUpdate' + CapCase(cursor.IntfName) + 'ByKey');

          mapper.Methods.Add(mapperMethod);
          mapper.Fields.Add(mapperField);

          mapperField.ReferencedType := aContext.getNamedItem('IZPreparedStatement');
          mapperField.TypeName := 'IZPreparedStatement';
          mapperField.Visibility := vlProtected;
          mapperField.InitializiationValue := generaUpdateSQLPerPrimaryKey(cursor);
          mapperMethod.Visibility := vlPublic;
          mapperMethod.SetterOf := mapperField;
          mapperParameter := TOMParameter.Create('entity');
          mapperParameter.ParameterTypeName := cursor.IntfName;
          mapperParameter.ParameterProtocolo := ppConst;

          mapperMethod.Parameters.Add(mapperParameter);
        end;

        if cursor.PrimaryKey.Columns.Count > 0 then
        begin
          mapperMethod := TOMMethod.Create('remove' + CapCase(cursor.IntfName));
          mapperField  := TOMField.Create('FDelete' + CapCase(cursor.IntfName) + 'ByKey');

          mapper.Methods.Add(mapperMethod);
          mapper.Fields.Add(mapperField);

          mapperField.ReferencedType := aContext.getNamedItem('IZPreparedStatement');
          mapperField.TypeName := 'IZPreparedStatement';
          mapperField.Visibility := vlProtected;
          mapperField.InitializiationValue := generaDeleteSQLPerPrimaryKey(cursor);
          mapperMethod.Visibility := vlPublic;
          mapperMethod.SetterOf := mapperField;
          mapperParameter := TOMParameter.Create('entity');
          mapperParameter.ParameterTypeName := cursor.IntfName;
          mapperParameter.ParameterProtocolo := ppConst;

          mapperMethod.Parameters.Add(mapperParameter);
        end;

        if cursor.PrimaryKey.Columns.Count > 0 then
        begin
          mapperMethod := TOMMethod.Create('create' + CapCase(cursor.IntfName));
          mapperField  := TOMField.Create('FInsert' + CapCase(cursor.IntfName) + 'ByKey');

          mapper.Methods.Add(mapperMethod);
          mapper.Fields.Add(mapperField);

          mapperField.ReferencedType := aContext.getNamedItem('IZPreparedStatement');
          mapperField.TypeName := 'IZPreparedStatement';
          mapperField.Visibility := vlProtected;
          mapperField.InitializiationValue := generaInsertSQLPerPrimaryKey(cursor);
          mapperMethod.Visibility := vlPublic;
          mapperMethod.SetterOf := mapperField;
          mapperParameter := TOMParameter.Create('entity');
          mapperParameter.ParameterTypeName := cursor.IntfName;
          mapperParameter.ParameterProtocolo := ppConst;

          mapperMethod.Parameters.Add(mapperParameter);
        end;

      end;
    end
    else
    begin
      DebugLn('prepareMappers:', cursor.CompilationUnitName, ' not found!');
    end;
  end;
  DebugLnExit('prepareMappers');
end;

procedure TlzBatisGenerator.processaContesto(const aContext: TConfigurationContext);
begin
  DebugLnEnter('ProcessaContesto');
  DebugLn(aContext.Id);
  prepareCompilationUnit(aContext);
  prepareEntities(aContext);
  prepareInterfaces(aContext);
  prepareMappers(aContext);
  DebugLnExit('ProcessaContesto');
end;

procedure TlzBatisGenerator.scriviContesto(const aContext: TConfigurationContext);
var
  compilationUnit: TOMCompilationUnit;
  uw: TUnitWriter;
begin
  uw := TUnitWriter.Create;
  uw.CurrentContext := aContext;
  for compilationUnit in aContext.CompilationUnits do
  begin
    if compilationUnit.UnitName <> '' then
    begin
      uw.generate(compilationUnit);
    end;
  end;
  uw.Free;
end;

procedure TlzBatisGenerator.disattivaConnessione;
begin
  FConnection.Close;
end;

function TlzBatisGenerator.leggiColonna(node: TDOMNode): TConfigurationColumn;
begin
  DebugLnEnter('Config.Context.Table.Column');
  Result      := TConfigurationColumn.Create;
  Result.ColumnName := node.attr('column-name');
  Result.ColumnType := node.attr('column-type');
  Result.ModelName := node.attr('model-name');
  Result.ModelType := node.attr('model-type');
  Result.Skip := lowercase(node.attr('skip')) = 'true';
  DebugLn(Result.ColumnName, ':', Result.ColumnType, ' -> ', Result.ModelName, ':', Result.ModelType);
  DebugLnExit('Config.Context.Table.Column');
end;

function TlzBatisGenerator.leggiMapperMethod(node: TDOMNode): TConfigurationMapperMethod;
var
  n: TDOMNode;
  p: TConfigurationParameter;
begin
  DebugLnEnter('Config.Context.MapperMethod');
  Result := TConfigurationMapperMethod.Create;
  Result.MethodName := node.attr('name');
  Result.BodyMethod := node.TextContent;
  for n in node do
  begin
    if LowerCase(n.NodeName) = 'parameter' then
    begin
      p := TConfigurationParameter.Create;
      p.ParameterName := n.attr('name');
      p.ParameterType := n.attr('type');
      Result.Parameters.Add(p);
    end
    else
    if LowerCase(n.NodeName) = 'return' then
    begin
      Result.ResultName := n.attr('type');
    end;
  end;
  DebugLnExit('Config.Context.MapperMethod');
end;

procedure TlzBatisGenerator.leggiConfigurazioneMapper(node: TDOMNode; mapperConfig: TConfigurationMapper);
var
  method: TConfigurationMapperMethod;
  n: TDOMNode;
begin
  DebugLnEnter('Config.Context.Mapper');
  for n in node do
  begin
    if LowerCase(n.NodeName) = 'select' then
    begin
      method := leggiMapperMethod(n);
      mapperConfig.methods.Add(method);
    end
    else
    if LowerCase(n.NodeName) = 'delete' then
    begin
      method := leggiMapperMethod(n);
      mapperConfig.methods.Add(method);
    end
    else
    if LowerCase(n.NodeName) = 'insert' then
    begin
      method := leggiMapperMethod(n);
      mapperConfig.methods.Add(method);
    end
    else
    if LowerCase(n.NodeName) = 'update' then
    begin
      method := leggiMapperMethod(n);
      mapperConfig.methods.Add(method);
    end;
  end;
  DebugLnExit('Config.Context.Mapper');
end;

function TlzBatisGenerator.leggiTabella(node: TDOMNode): TConfigurationTable;
var
  n: TDOMNode;
begin
  DebugLnEnter('Config.Context.Table');
  Result := TConfigurationTable.Create;
  Result.TableName := node.attr('table-name');
  Result.MapperName := node.attr('mapper-name');
  Result.IntfName := node.attr('entity-name');
  Result.ImplName := node.attr('implementation-name');
  Result.CompilationUnitName := node.attr('target-module');
  DebugLn(Result.TableName, ' ', Result.ImplName, ' ', Result.IntfName, ' ', Result.MapperName);
  for n in node do
  begin
    if LowerCase(n.NodeName) = 'column' then
    begin
      Result.Columns.Add(leggiColonna(n));
    end
    else if LowerCase(n.NodeName) = 'mapper' then
    begin
      LeggiConfigurazioneMapper(n, Result.MapperConfiguration);
    end;
  end;
  DebugLnExit('Config.Context.Table');
end;

function TlzBatisGenerator.generaSelectSQLPerPrimaryKey(tabella: TConfigurationTable): string;
var
  colonna: TConfigurationColumn;
begin
  Result := 'SELECT ';
  for colonna in tabella.Columns do
  begin
    if tabella.Columns.IndexOf(colonna) > 0 then
    begin
      Result += ',';
    end;
    Result += colonna.ColumnName;
  end;
  Result += ' FROM ';
  Result += tabella.TableName;
  if tabella.PrimaryKey.Columns.Count > 0 then
  begin
    Result += ' WHERE';
    for colonna in tabella.PrimaryKey.Columns do
    begin
      if tabella.PrimaryKey.Columns.IndexOf(colonna) > 0 then
      begin
        Result += ' AND';
      end;
      Result += ' ' + colonna.ColumnName + ' = ${a' + colonna.ModelName + '}';
    end;
  end;
end;

function TlzBatisGenerator.generaUpdateSQLPerPrimaryKey(tabella: TConfigurationTable): string;
var
  colonna: TConfigurationColumn;
  numeroColonne: integer = 0;
begin
  Result := 'UPDATE ';
  Result += tabella.TableName;
  Result += ' SET';
  for colonna in tabella.Columns do
  begin
    if tabella.PrimaryKey.Columns.IndexOf(colonna) > -1 then
    begin
      continue;
    end;
    if numeroColonne > 0 then
    begin
      Result += ',';
    end;
    Result += ' ' + colonna.ColumnName + ' = ${entity.' + colonna.ModelName + '}';
    Inc(numeroColonne);
  end;
  if tabella.PrimaryKey.Columns.Count > 0 then
  begin
    Result += ' WHERE';
    for colonna in tabella.PrimaryKey.Columns do
    begin
      if tabella.PrimaryKey.Columns.IndexOf(colonna) > 0 then
      begin
        Result += ' AND';
      end;
      Result += ' ' + colonna.ColumnName + ' = ${entity.' + colonna.ModelName + '}';
    end;
  end;
end;

function TlzBatisGenerator.generaDeleteSQLPerPrimaryKey(tabella: TConfigurationTable): string;
var
  colonna: TConfigurationColumn;
  numeroColonne: integer = 0;
begin
  Result := 'DELETE FROM ';
  Result += tabella.TableName;
  if tabella.PrimaryKey.Columns.Count > 0 then
  begin
    Result += ' WHERE ';
    for colonna in tabella.PrimaryKey.Columns do
    begin
      if numeroColonne > 0 then
      begin
        Result += ' AND ';
      end;
      Result += ' ' + colonna.ColumnName + ' = ${entity.' + colonna.ModelName + '}';
      Inc(numeroColonne);
    end;
  end;
end;

function TlzBatisGenerator.generaInsertSQLPerPrimaryKey(tabella: TConfigurationTable): string;
var
  colonna: TConfigurationColumn;
  numeroColonne: integer = 0;
begin
  Result := 'INSERT INTO ';
  Result += tabella.TableName;
  Result += '(';
  for colonna in tabella.Columns do
  begin
    if tabella.PrimaryKey.Columns.IndexOf(colonna) > -1 then
    begin
      continue;
    end;
    if numeroColonne > 0 then
    begin
      Result += ',';
    end;
    Result += colonna.ColumnName;
    Inc(numeroColonne);
  end;
  Result += ') VALUES (';
  numeroColonne := 0;
  for colonna in tabella.Columns do
  begin
    if numeroColonne > 0 then
    begin
      Result += ',';
    end;
    Result += ' ${entity.' + colonna.ModelName + '}';
    Inc(numeroColonne);
  end;
  Result += ')';
end;

function TlzBatisGenerator.leggiConnessione(node: TDOMNode): TConfigurationConnection;
begin
  DebugLnEnter('Config.Context.Connection');
  Result     := TConfigurationConnection.Create;
  Result.Url := node.attr('connection-url');
  Result.UserName := node.attr('username');
  Result.Password := node.attr('password');
  DebugLnExit('Config.Context.Connection');
end;

procedure TlzBatisGenerator.leggiContesto(node: TDOMNode);
var
  aContext: TConfigurationContext;
  n: TDOMNode;
begin
  DebugLnEnter('config.context');
  aContext := TConfigurationContext.Create(self);
  FConfiguration.Contextes.Add(aContext);
  aContext.Id := node.attr('id');
  aContext.targetLocation := node.attr('target-location');
  for n in node do
  begin
    if lowercase(n.NodeName) = 'zdbc-connection' then
    begin
      aContext.zdbcConnection := leggiConnessione(n);
    end;
    if lowercase(n.NodeName) = 'table' then
    begin
      aContext.Tables.Add(leggiTabella(n));
    end;
    if LowerCase(n.NodeName) = 'copyright' then
    begin
      aContext.CopyRight := n.TextContent;
    end;
  end;
  DebugLnExit('config.context');
end;

procedure TlzBatisGenerator.leggiConfigurazione(document: TXMLDocument);
var
  n, c: TDOMNode;
begin
  DebugLnEnter('Read XML File');
  FConfiguration := TConfiguration.Create(Self);
  n := document.DocumentElement;
  for c in n do
  begin
    if lowercase(c.NodeName) = 'context' then
    begin
      leggiContesto(c);
    end;
  end;
  DebugLnExit('Read XML File');
end;

procedure TlzBatisGenerator.DoRun;
var
  xmlDocument: TXMLDocument;
  context: TConfigurationContext;
begin
  if FFileNameInput <> '' then
  begin
    ReadXMLFile(xmlDocument, FFileNameInput);
    leggiConfigurazione(xmlDocument);
    FCompliationUnits := TOMCompilationUnits.Create(True);
    for context in FConfiguration.Contextes do
    begin
      attivaConnessione(context);
      completaConfigurazione(context);
      processaContesto(context);
      scriviContesto(context);
      disattivaConnessione;
    end;
    FCompliationUnits.Free;
  end
  else
  begin
    Writeln('Missing required parameter: -configfile');
    Writeln(ExtractFileName(ParamStr(0)));
    Writeln(' -configfile file_name (required) Specifies the name of the configuration file.');
    Writeln(' -overwrite (optional) 	If specified, then existing files will be overwritten if an existing file if found with the same name as a generated file. If not specified, and a file already exists with the same name as a generated file, then MBG will write the newly generated Java file to the proper directory with a unique name (e.g. target-file.1, target-file.2, etc.). Important: The generator will always merge and overwrite XML files.');
    //Writeln(' -contextids context1,context2,...(optional)');
    //Writeln(' -tables table1, table2,... (optional)');
  end;
  Terminate;
end;

constructor TlzBatisGenerator.Create(TheOwner: TComponent);
var
  idx: integer = 1;
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FOverride := False;
  FTables   := TStringList.Create;
  repeat
    if ParamStr(idx) = '-configfile' then
    begin
      FFileNameInput := ParamStr(idx + 1);
      Inc(idx);
      Continue;
    end;
    if ParamStr(idx) = '-override' then
    begin
      FOverride := True;
    end;
    if ParamStr(idx) = '-tables' then
    begin
      Inc(idx);
      FTables.CommaText := ParamStr(idx);
    end;
    Inc(idx);
  until idx > ParamCount;
end;

destructor TlzBatisGenerator.Destroy;
begin
  inherited Destroy;
end;

end.
