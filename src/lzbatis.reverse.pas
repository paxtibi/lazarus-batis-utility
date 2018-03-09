unit lzbatis.reverse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZDbcIntfs, ZDbcUtils;

function showTable(connection: IZConnection; Name: string): string;

implementation

uses
  strutils, paxtibi.utils;

function showTable(connection: IZConnection; Name: string): string;
var
  ps: TPrintStream;
  metadata: IZResultSet;
  mm: TMemoryStream;
  columnType: TZSQLType;
begin
  mm := TMemoryStream.Create;
  ps := TPrintStream.Create(mm);
  metadata := connection.GetMetadata.GetColumns('', '', Name, '%');
  ps.print('CREATE TABLE ').print(Name).print(' (').println();
  while metadata.Next do
  begin
    ps.print(' "').print(metadata.getStringByName('COLUMN_NAME')).print('"');
    columnType := TZSQLType(metadata.GetSmallByName('DATA_TYPE'));
    ps.print(' ').print(metadata.GetStringByName('TYPE_NAME')).print(' ');
    if not metadata.IsNullByName('COLUMN_SIZE') then
    begin
      case columnType of
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
        stFloat, stDouble, stCurrency, stBigDecimal,
        stString, stUnicodeString,
        stBytes, stArray:
        begin
          ps.print('(').print(metadata.GetStringByName('COLUMN_SIZE'));
          if (not metadata.IsNullByName('DECIMAL_DIGITS') and (metadata.GetIntByName('DECIMAL_DIGITS') > 0)) then
          begin
            ps.print(',').print(metadata.GetIntByName('DECIMAL_DIGITS'));
          end;
          ps.print(')');
        end;
      end;
    end;
    case TZColumnNullableType(metadata.GetIntByName('NULLABLE')) of
      ntNoNulls:
      begin
        ps.print(' NOT NULL ');
      end;
      ntNullable,
      ntNullableUnknown:
      begin
      end;
    end;
    if not metadata.IsNullByName('COLUMN_DEF') then
    begin
      ps.print(' default "').print(metadata.GetStringByName('COLUMN_DEF')).print('"');
    end;
    ps.print(',');
    ps.println();
  end;
  mm.seek(-(Length(LineEnding) + 1), soCurrent);
  ps.print(LineEnding).print('); ').println();

  SetString(Result, PChar(mm.Memory), mm.Size);
  FreeAndNil(ps);
  FreeAndNil(mm);
end;

end.
