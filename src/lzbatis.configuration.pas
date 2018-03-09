unit lzbatis.configuration;

{$mode objfpc}{$H+}

interface

uses
  Laz2_DOM, Classes, SysUtils, ZDbcIntfs;

type

  { TConfigurationPreparator }

  TConfigurationPreparator = class
  private
    fDocument: TXMLDocument;
    FConnection: IZConnection;
    FConnectionString: string;
    FPassword: string;
    FTargetFile: string;
    FUserName: string;
    procedure SetConnectionString(AValue: string);
    procedure SetPassword(AValue: string);
    procedure SetTargetFile(AValue: string);
    procedure SetUserName(AValue: string);
  protected
    procedure processColumns(targetNode: TDOMNode; tableName: string);
    procedure processTables(targetNode: TDOMNode);
  public
    procedure Prepare;
    property TargetFile: string read FTargetFile write SetTargetFile;
    property ConnectionString: string read FConnectionString write SetConnectionString;
    property UserName: string read FUserName write SetUserName;
    property Password: string read FPassword write SetPassword;
  end;

  { TConfigurationReverse }

  TConfigurationReverse = class
  private
    FConnectionString: string;
    FPassword: string;
    FTargetFile: string;
    FUserName: string;
    FConnection: IZConnection;
    procedure SetConnectionString(AValue: string);
    procedure SetPassword(AValue: string);
    procedure SetTargetFile(AValue: string);
    procedure SetUserName(AValue: string);
  public
    procedure reverse;
    property TargetFile: string read FTargetFile write SetTargetFile;
    property ConnectionString: string read FConnectionString write SetConnectionString;
    property UserName: string read FUserName write SetUserName;
    property Password: string read FPassword write SetPassword;
  end;


implementation

uses
  laz2_XMLWrite, laz2_XMLRead, lzBatis.dom.aspects, paxtibi.utils, lzBatis.reverse;

{ TConfigurationReverse }

procedure TConfigurationReverse.SetConnectionString(AValue: string);
begin
  if FConnectionString = AValue then
  begin
    Exit;
  end;
  FConnectionString := AValue;
end;

procedure TConfigurationReverse.SetPassword(AValue: string);
begin
  if FPassword = AValue then
  begin
    Exit;
  end;
  FPassword := AValue;
end;

procedure TConfigurationReverse.SetTargetFile(AValue: string);
begin
  if FTargetFile = AValue then
  begin
    Exit;
  end;
  FTargetFile := AValue;
end;

procedure TConfigurationReverse.SetUserName(AValue: string);
begin
  if FUserName = AValue then
  begin
    Exit;
  end;
  FUserName := AValue;
end;

procedure TConfigurationReverse.reverse;
var
  Target: TPrintStream;
  rs: IZResultSet;
  tableTypes: array of string;
begin
  SetLength(tableTypes, 1);
  tableTypes[0] := 'TABLE';
  Writeln(ConnectionString);
  FConnection := DriverManager.GetConnectionWithLogin(ConnectionString, UserName, Password);
  Target := TPrintStream.Create(TFileStream.Create(FTargetFile, fmCreate));
  rs := FConnection.GetMetadata.GetTables('', '', '%', tableTypes);
  while rs.Next do
  begin
    Target.print(showTable(FConnection, rs.getStringByName('TABLE_NAME')))
      .println();
  end;
  FreeAndNil(Target);
end;

{ TConfigurationPreparator }

procedure TConfigurationPreparator.SetConnectionString(AValue: string);
begin
  if FConnectionString = AValue then
  begin
    Exit;
  end;
  FConnectionString := AValue;
end;

procedure TConfigurationPreparator.SetPassword(AValue: string);
begin
  if FPassword = AValue then
  begin
    Exit;
  end;
  FPassword := AValue;
end;

procedure TConfigurationPreparator.SetTargetFile(AValue: string);
begin
  if FTargetFile = AValue then
  begin
    Exit;
  end;
  FTargetFile := AValue;
end;

procedure TConfigurationPreparator.SetUserName(AValue: string);
begin
  if FUserName = AValue then
  begin
    Exit;
  end;
  FUserName := AValue;
end;

procedure TConfigurationPreparator.processColumns(targetNode: TDOMNode; tableName: string);
var
  rs: IZResultSet;
  column: TDOMNode;
begin
  rs := FConnection.GetMetadata.GetColumns('', '', tableName, '%');
  while rs.Next do
  begin
    column := fDocument.CreateElement('column');
    targetNode.AppendChild(column);
    column.attr('column-name', rs.GetStringByName('COLUMN_NAME'));
    column.attr('column-type', rs.GetStringByName('TYPE_NAME'));
    column.attr('column-default', rs.GetStringByName('COLUMN_DEF'));
    column.attr('model-name', rs.GetStringByName('COLUMN_NAME'));
    column.attr('model-type', rs.GetStringByName('TYPE_NAME'));
  end;
end;

procedure TConfigurationPreparator.processTables(targetNode: TDOMNode);
var
  rs: IZResultSet;
  tableTypes: array of string;
  tableNode, comment: TDOMNode;
  tableName: string;
begin
  SetLength(tableTypes, 1);
  tableTypes[0] := 'TABLE';
  rs := FConnection.GetMetadata.GetTables('', '', '%', tableTypes);
  while rs.Next do
  begin
    tableName := rs.GetStringByName('TABLE_NAME');

    tableNode := fDocument.CreateElement('table');
    comment   := fDocument.CreateComment('if you want skip then table add skip=true to table definition and remove columns below');
    targetNode.AppendChild(tableNode);
    tableNode.AppendChild(comment);
    tableNode.attr('table-name', tableName);
    tableNode.attr('mapper-name', 'T' + tableName + 'Mapper');
    tableNode.attr('entity-name', 'I' + tableName);
    tableNode.attr('implementation-name', 'T' + tableName);
    tableNode.attr('target-module', tableName);
    processColumns(tableNode, tableName);
  end;
end;

procedure TConfigurationPreparator.Prepare;
var
  root: TDOMNode;
  uniqueContext, zdbcConnection: TDOMNode;
begin
  FDocument := TXMLDocument.Create;
  root      := FDocument.CreateElement('generator-configuration');
  uniqueContext := FDocument.CreateElement('context');
  zdbcConnection := FDocument.CreateElement('zdbc-connection');
  zdbcConnection.attr('connection-url', ConnectionString);
  zdbcConnection.attr('username', UserName);
  zdbcConnection.attr('password', Password);
  if ConnectionString.Contains('firebird') then
  begin
    zdbcConnection.attr('skip', '(RDB|SEC|MON)\$(.*)');
  end;
  FDocument.AppendChild(root);
  root.AppendChild(uniqueContext);
  uniqueContext.AppendChild(zdbcConnection);
  FConnection := DriverManager.GetConnectionWithLogin(ConnectionString, UserName, Password);
  processTables(uniqueContext);
  WriteXMLFile(FDocument, TargetFile, []);
  FreeAndNil(FDocument);
end;

end.
