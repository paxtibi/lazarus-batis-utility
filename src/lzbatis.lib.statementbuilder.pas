unit lzbatis.lib.statementbuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStatementType = (stUndefined, stDELETE, stINSERT, stSELECT, stUPDATE);

  TSQLStatement = class;
  TSQLStatementArray = array of TSQLStatement;

  { TSQLStatement }

  TSQLStatement = class(TInterfacedObject)
  protected
    statementType: TStatementType;
    sets: TStrings;//= TArrayList.Create();
    select: TStrings;//= TArrayList.Create();
    tables: TStrings;//= TArrayList.Create();
    join: TStrings;//= TArrayList.Create();
    innerJoin: TStrings;//= TArrayList.Create();
    outerJoin: TStrings;//= TArrayList.Create();
    leftOuterJoin: TStrings;//= TArrayList.Create();
    rightOuterJoin: TStrings;//= TArrayList.Create();
    where: TStrings;//= TArrayList.Create();
    having: TStrings;//= TArrayList.Create();
    groupBy: TStrings;//= TArrayList.Create();
    orderBy: TStrings;//= TArrayList.Create();
    lastList: TStrings;//= TArrayList.Create();
    columns: TStrings;//= TArrayList.Create();
    values: TStrings;//= TArrayList.Create();
    distinct: boolean;
  private
    procedure sqlClause(var builder: string; const keyword: string; const parts: TStrings; const Open: string; const Close: string; const conjunction: string); overload; virtual;
    function selectSQL(var buffer: string): string; overload; virtual;
    function insertSQL(var builder: string): string; overload; virtual;
    function deleteSQL(var builder: string): string; overload; virtual;
    function updateSQL(var builder: string): string; overload; virtual;
  public
    constructor Create(); overload; virtual;
    destructor Destroy; override;
    function generate(var buffer: string): string; overload; virtual;
  end;

  TStatementBuilder = class;
  TAbstractSQLArray = array of TStatementBuilder;

  { TStatementBuilder }

  TStatementBuilder = class  abstract  (TInterfacedObject)
  private
    fsql: TSQLStatement;// = TSQLStatement.Create();
  protected
    property sql: TSQLStatement read fsql;
  public
    constructor Create(); overload; virtual;
    destructor Destroy; override;
    function DELETE_FROM(const table: string): TStatementBuilder; overload; virtual;
    function FROM(const table: string): TStatementBuilder; overload; virtual;
    function FROM(const tables: array of string): TStatementBuilder; overload; virtual;
    function getSelf(): TStatementBuilder; overload; virtual;
    function GROUP_BY(const columns: array of string): TStatementBuilder; overload; virtual;
    function GROUP_BY(const columns: string): TStatementBuilder; overload; virtual;
    function HAVING(const conditions: array of string): TStatementBuilder; overload; virtual;
    function HAVING(const conditions: string): TStatementBuilder; overload; virtual;
    function INNER_JOIN(const aJoin: string): TStatementBuilder; overload; virtual;
    function INNER_JOIN(const joins: array of string): TStatementBuilder; overload; virtual;
    function INSERT_INTO(const tableName: string): TStatementBuilder; overload; virtual;
    function INTO_COLUMNS(const columns: array of string): TStatementBuilder; overload; virtual;
    function INTO_VALUES(const aValues: array of string): TStatementBuilder; overload; virtual;
    function JOIN(const joinExpression: string): TStatementBuilder; overload; virtual;
    function JOIN(const joins: array of string): TStatementBuilder; overload; virtual;
    function LEFT_OUTER_JOIN(const aJoin: string): TStatementBuilder; overload; virtual;
    function LEFT_OUTER_JOIN(const joins: array of string): TStatementBuilder; overload; virtual;
    function ORDER_BY(const columns: array of string): TStatementBuilder; overload; virtual;
    function ORDER_BY(const columns: string): TStatementBuilder; overload; virtual;
    function OUTER_JOIN(const aJoin: string): TStatementBuilder; overload; virtual;
    function OUTER_JOIN(const joins: array of string): TStatementBuilder; overload; virtual;
    function RIGHT_OUTER_JOIN(const aJoin: string): TStatementBuilder; overload; virtual;
    function RIGHT_OUTER_JOIN(const joins: array of string): TStatementBuilder; overload; virtual;
    function SELECT(const columns: array of string): TStatementBuilder; overload; virtual;
    function SELECT(const columns: string): TStatementBuilder; overload; virtual;
    function SELECT_DISTINCT(const columns: array of string): TStatementBuilder; overload; virtual;
    function SELECT_DISTINCT(const columns: string): TStatementBuilder; overload; virtual;
    function toString(): ansistring; overload; override;
    function UPDATE(const table: string): TStatementBuilder; overload; virtual;
    function usingAppender(var appendable: string): string; overload; virtual;
    function VALUES(const column: string; const Value: string): TStatementBuilder; overload; virtual;
    function WHERE(const conditions: array of string): TStatementBuilder; overload; virtual;
    function WHERE(const conditions: string): TStatementBuilder; overload; virtual;
    function _AND(): TStatementBuilder; overload; virtual;
    function _OR(): TStatementBuilder; overload; virtual;
    function _SET(const sets: array of string): TStatementBuilder; overload; virtual;
    function _SET(const sets: string): TStatementBuilder; overload; virtual;
  end;

implementation

const
  AND_VALUE: string = ') ' + LineEnding + 'AND (';
  OR_VALUE: string = ') ' + LineEnding + 'OR (';


{ TStatementBuilder }

constructor TStatementBuilder.Create();
begin
  fsql := TSQLStatement.Create();
end;

destructor TStatementBuilder.Destroy;
begin
  FreeAndNil(fsql);
  inherited Destroy;
end;

function TStatementBuilder.DELETE_FROM(const table: string): TStatementBuilder;
begin
  sql.statementType := TStatementType.stDelete;
  sql.tables.add(table);
  exit(getSelf());
end;

function TStatementBuilder.FROM(const table: string): TStatementBuilder;
begin
  sql.tables.add(table);
  exit(getSelf());
end;

function TStatementBuilder.FROM(const tables: array of string): TStatementBuilder;
var
  table: string;
begin
  for table in tables do
  begin
    sql.tables.Add(table);
  end;
  exit(getSelf());
end;

function TStatementBuilder.getSelf(): TStatementBuilder;
begin
  Result := self;
end;

function TStatementBuilder.GROUP_BY(const columns: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in columns do
    sql.groupBy.add(Value);
  exit(getSelf());
end;

function TStatementBuilder.GROUP_BY(const columns: string): TStatementBuilder;
begin
  sql.groupBy.add(columns);
  exit(getSelf());
end;

function TStatementBuilder.HAVING(const conditions: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in conditions do
    sql.having.add(Value);
  sql.lastList := sql.having;
  exit(getSelf());
end;

function TStatementBuilder.HAVING(const conditions: string): TStatementBuilder;
begin
  sql.having.add(conditions);
  sql.lastList := sql.having;
  exit(getSelf());
end;

function TStatementBuilder.INNER_JOIN(const aJoin: string): TStatementBuilder;
begin
  sql.innerJoin.add(aJoin);
  exit(getSelf());
end;

function TStatementBuilder.INNER_JOIN(const joins: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in joins do
    sql.innerJoin.add(Value);
  exit(getSelf());
end;

function TStatementBuilder.INSERT_INTO(const tableName: string): TStatementBuilder;
begin
  sql.statementType := TStatementType.stINSERT;
  sql.tables.add(tableName);
  exit(getSelf());
end;

function TStatementBuilder.INTO_COLUMNS(const columns: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in columns do
    sql.columns.Add(Value);
  exit(getSelf());
end;

function TStatementBuilder.INTO_VALUES(const aValues: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in aValues do
    sql.values.add(Value);
  exit(getSelf());
end;

function TStatementBuilder.JOIN(const joinExpression: string): TStatementBuilder;
begin
  sql.join.add(joinExpression);
  exit(getSelf());
end;

function TStatementBuilder.JOIN(const joins: array of string): TStatementBuilder;
var
  table: string;
begin
  for table in joins do
    sql.join.add(table);
  exit(getSelf());
end;

function TStatementBuilder.LEFT_OUTER_JOIN(const aJoin: string): TStatementBuilder;
begin
  sql.leftOuterJoin.add(aJoin);
  exit(getSelf());
end;

function TStatementBuilder.LEFT_OUTER_JOIN(const joins: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in joins do
    sql.leftOuterJoin.add(Value);
  exit(getSelf());
end;

function TStatementBuilder.ORDER_BY(const columns: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in columns do
  begin
    sql.orderBy.add(Value);
  end;
  exit(getSelf());
end;

function TStatementBuilder.ORDER_BY(const columns: string): TStatementBuilder;
begin
  sql.orderBy.add(columns);
  exit(getSelf());
end;

function TStatementBuilder.OUTER_JOIN(const aJoin: string): TStatementBuilder;
begin
  sql.outerJoin.add(aJoin);
  exit(getSelf());
end;

function TStatementBuilder.OUTER_JOIN(const joins: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in joins do
  begin
    sql.outerJoin.add(Value);
  end;
  exit(getSelf());
end;

function TStatementBuilder.RIGHT_OUTER_JOIN(const aJoin: string): TStatementBuilder;
begin
  sql.rightOuterJoin.add(aJoin);
  exit(getSelf());
end;

function TStatementBuilder.RIGHT_OUTER_JOIN(const joins: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in joins do
  begin
    sql.rightOuterJoin.add(Value);
  end;
  exit(getSelf());
end;

function TStatementBuilder.SELECT(const columns: array of string): TStatementBuilder;
var
  Value: string;
begin
  sql.statementType := TStatementType.stSELECT;
  for Value in columns do
  begin
    sql.select.add(Value);
  end;
  exit(getSelf());
end;

function TStatementBuilder.SELECT(const columns: string): TStatementBuilder;
begin
  sql.statementType := TStatementType.stSELECT;
  sql.select.add(columns);
  exit(getSelf());
end;

function TStatementBuilder.SELECT_DISTINCT(const columns: array of string): TStatementBuilder;
var
  Value: string;
begin
  sql.distinct := True;
  sql.statementType := TStatementType.stSELECT;
  for Value in columns do
  begin
    sql.select.add(Value);
  end;
  exit(getSelf());
end;

function TStatementBuilder.SELECT_DISTINCT(const columns: string): TStatementBuilder;
var
  Value: string;
begin
  sql.distinct := True;
  sql.statementType := TStatementType.stSELECT;
  for Value in columns do
  begin
    sql.select.add(Value);
  end;
  exit(getSelf());
end;

function TStatementBuilder.toString(): ansistring;
begin
  sql.generate(Result);
end;

function TStatementBuilder.UPDATE(const table: string): TStatementBuilder;
begin
  sql.statementType := TStatementType.stUPDATE;
  sql.tables.add(table);
  exit(getSelf());
end;

function TStatementBuilder.usingAppender(var appendable: string): string;
begin
  sql.generate(appendable);
  exit(appendable);
end;

function TStatementBuilder.VALUES(const column: string; const Value: string): TStatementBuilder;
begin
  sql.columns.add(column);
  sql.values.add(Value);
  exit(getSelf());
end;

function TStatementBuilder.WHERE(const conditions: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in conditions do
    sql.where.add(Value);
  sql.lastList := sql.where;
  exit(getSelf());
end;

function TStatementBuilder.WHERE(const conditions: string): TStatementBuilder;
begin
  sql.where.add(conditions);
  sql.lastList := sql.where;
  exit(getSelf());
end;

function TStatementBuilder._AND(): TStatementBuilder;
begin
  sql.lastList.add(AND_VALUE);
  exit(getSelf());
end;

function TStatementBuilder._OR(): TStatementBuilder;
begin
  sql.lastList.add(OR_VALUE);
  exit(getSelf());
end;

function TStatementBuilder._SET(const sets: array of string): TStatementBuilder;
var
  Value: string;
begin
  for Value in sets do
    sql.sets.add(Value);
  Exit(getSelf());
end;

function TStatementBuilder._SET(const sets: string): TStatementBuilder;
begin
  sql.sets.add(sets);
  Exit(getSelf());
end;

{ TSQLStatement }

procedure TSQLStatement.sqlClause(var builder: string; const keyword: string; const parts: TStrings; const Open: string; const Close: string; const conjunction: string);
var
  last: string;
  part: string;
  i: integer;
begin
  if parts.Count <> 0 then
  begin
    if Length(builder) > 0 then
    begin
      builder += LineEnding;
    end;
    builder += keyword;
    builder += (' ');
    builder += Open;
    last := '________';
    I := 0;
    for part in parts do
    begin
      if (i > 0) and (not part.equals(AND_VALUE)) and (not part.equals(OR_VALUE)) and (not last.equals(AND_VALUE)) and (not last.equals(OR_VALUE)) then
      begin
        builder += conjunction;
      end;
      builder += part;
      last := part;
      Inc(i);
    end;
    builder += Close;
  end;
end;

function TSQLStatement.selectSQL(var buffer: string): string;
begin
  if distinct then
  begin
    sqlClause(buffer, 'SELECT DISTINCT', select, '', '', ', ');
  end
  else
  begin
    sqlClause(buffer, 'SELECT', select, '', '', ', ');
  end;
  sqlClause(buffer, 'FROM', tables, '', '', ', ');
  sqlClause(buffer, 'JOIN', join, '', '', LineEnding + 'JOIN ');
  sqlClause(buffer, 'INNER JOIN', innerJoin, '', '', LineEnding + 'INNER JOIN ');
  sqlClause(buffer, 'OUTER JOIN', outerJoin, '', '', LineEnding + 'OUTER JOIN ');
  sqlClause(buffer, 'LEFT OUTER JOIN', leftOuterJoin, '', '', LineEnding + 'LEFT OUTER JOIN ');
  sqlClause(buffer, 'RIGHT OUTER JOIN', rightOuterJoin, '', '', LineEnding + 'RIGHT OUTER JOIN ');
  sqlClause(buffer, 'WHERE', where, '(', ')', ' AND ');
  sqlClause(buffer, 'GROUP BY', groupBy, '', '', ', ');
  sqlClause(buffer, 'HAVING', having, '(', ')', ' AND ');
  sqlClause(buffer, 'ORDER BY', orderBy, '', '', ', ');
  exit(buffer);
end;

function TSQLStatement.insertSQL(var builder: string): string;
begin
  sqlClause(builder, 'INSERT INTO', tables, '', '', '');
  sqlClause(builder, '', columns, '(', ')', ', ');
  sqlClause(builder, 'VALUES', values, '(', ')', ', ');
  exit(builder);
end;

function TSQLStatement.deleteSQL(var builder: string): string;
begin
  sqlClause(builder, 'DELETE FROM', tables, '', '', '');
  sqlClause(builder, 'WHERE', where, '(', ')', ' AND ');
  exit(builder);
end;

function TSQLStatement.updateSQL(var builder: string): string;
begin
  sqlClause(builder, 'UPDATE', tables, '', '', '');
  sqlClause(builder, 'SET', sets, '', '', ', ');
  sqlClause(builder, 'WHERE', where, '(', ')', ' AND ');
  exit(builder);
end;

constructor TSQLStatement.Create();
begin
  sets := TStringList.Create;
  select := TStringList.Create;
  tables := TStringList.Create;
  join := TStringList.Create;
  innerJoin := TStringList.Create;
  outerJoin := TStringList.Create;
  leftOuterJoin := TStringList.Create;
  rightOuterJoin := TStringList.Create;
  where := TStringList.Create;
  having := TStringList.Create;
  groupBy := TStringList.Create;
  orderBy := TStringList.Create;
  columns := TStringList.Create;
  values := TStringList.Create;
  distinct := False;
end;

destructor TSQLStatement.Destroy;
begin
  FreeAndNil(sets);
  FreeAndNil(select);
  FreeAndNil(tables);
  FreeAndNil(join);
  FreeAndNil(innerJoin);
  FreeAndNil(outerJoin);
  FreeAndNil(leftOuterJoin);
  FreeAndNil(rightOuterJoin);
  FreeAndNil(where);
  FreeAndNil(having);
  FreeAndNil(groupBy);
  FreeAndNil(orderBy);
  FreeAndNil(columns);
  FreeAndNil(values);
  inherited Destroy;
end;

function TSQLStatement.generate(var buffer: string): string;
begin
  Result := '';
  case (statementType) of
    stDELETE:
    begin
      Result := deleteSQL(buffer);
    end;
    stINSERT:
    begin
      Result := insertSQL(buffer);
    end;
    stSELECT:
    begin
      Result := selectSQL(buffer);
    end;
    stUPDATE:
    begin
      Result := updateSQL(buffer);
    end;
  end;
end;

procedure SelfTest;
const
  aspectedResult: string = 'SELECT P.ID, P.USERNAME, P.PASSWORD, P.FULL_NAME, P.LAST_NAME,P.CREATED_ON, P.UPDATED_ON ' + LineEnding +//
    'FROM PERSON P, ACCOUNT A ' + LineEnding +//
    'INNER JOIN DEPARTMENT D on D.ID = P.DEPARTMENT_ID ' + LineEnding +//
    'INNER JOIN COMPANY C on D.COMPANY_ID = C.ID ' + LineEnding +//
    'WHERE (P.ID = A.ID AND P.FIRST_NAME like ?) ' + LineEnding +//
    'OR (P.LAST_NAME like ?) ' + LineEnding +//
    'GROUP BY P.ID ' + LineEnding +//
    'HAVING (P.LAST_NAME like ?) ' + LineEnding +//
    'OR (P.FIRST_NAME like ?) ' + LineEnding +//
    'ORDER BY P.ID, P.FULL_NAME';
var
  s: TStatementBuilder;
begin
  s := TStatementBuilder.Create();
  with s do
  begin
    SELECT('P.ID, P.USERNAME, P.PASSWORD, P.FULL_NAME');
    SELECT('P.LAST_NAME, P.CREATED_ON, P.UPDATED_ON');
    FROM('PERSON P');
    FROM('ACCOUNT A');
    INNER_JOIN('DEPARTMENT D on D.ID = P.DEPARTMENT_ID');
    INNER_JOIN('COMPANY C on D.COMPANY_ID = C.ID');
    WHERE('P.ID = A.ID');
    WHERE('P.FIRST_NAME like ?');
    _OR();
    WHERE('P.LAST_NAME like ?');
    GROUP_BY('P.ID');
    HAVING('P.LAST_NAME like ?');
    _OR();
    HAVING('P.FIRST_NAME like ?');
    ORDER_BY('P.ID');
    ORDER_BY('P.FULL_NAME');
  end;
  Writeln(s.ToString);
  Writeln('=');
  Writeln(aspectedResult);
  Writeln(s.ToString = aspectedResult);
  s.Destroy;
end;

initialization

  SelfTest;

end.
