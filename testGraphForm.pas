unit testGraphForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs,
  basicGraph, DBACore, DBConn, sqlLoader;

type
  TForm1 = class(TForm)
    function GetGraph(): TBasicGraph;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;



var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.GetGraph(): TBasicGraph;
var
  queryString, connString: string;
  dbResult_Nodes, dbResult_Edges: TDBResult;
  conn: TDBAConnection;
begin
  Result := TbasicGraph.Create();
  
  connString := 'driver=MySql;hostname=uran;port=3313;user=mappl;password=gismgt;collation=cp1251_general_ci;charset=cp1251';

  conn := TDBACore.GetGlobalCore.addConnection( connString, False); 

  queryString := 'SELECT ' + conn.quoteName( 'MUID' ) + ' FROM ' + conn.quoteName( 'gis_mgt' )
                  + '.' + conn.quoteName( 'graph_nodes' );

  conn.QueryOpen( queryString, dbResult_Nodes, true );

  queryString := 'SELECT ' + conn.quoteName( 'MUID' ) + ', ' + conn.quoteName( 'startNodeMUID' )
                  + ', ' + conn.quoteName( 'endNodeMUID' ) + ', ' + conn.quoteName( 'ObjectLength' ) + ' FROM '
                  + conn.quoteName( 'gis_mgt' ) + '.' + conn.quoteName( 'graph_sections' );

  conn.QueryOpen( queryString, dbResult_Edges, true );

  while dbResult_Nodes.Fetch() do
    Result.AddNode( dbResult_Nodes.asInt64( 0 ) );

  while dbResult_Edges.Fetch() do
  begin
    if ( Result.GetNode_ByUID( dbResult_Edges.asInt64( 1 ) ) <> nil )
      AND ( Result.GetNode_ByUID( dbResult_Edges.asInt64( 2 ) ) <> nil ) then
      Result.AddEdge( Result.GetNode_ByUID( dbResult_Edges.asInt64( 1 ) ),
                    Result.GetNode_ByUID( dbResult_Edges.asInt64( 2 ) ),
                    dbResult_Edges.asInt64( 0 ),
                    dbResult_Edges.asFloat( 3 ) );
  end;

  FreeAndNil( dbResult_Nodes );
  FreeAndNil( dbResult_Edges );
  FreeAndNil( conn );
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  graph: TBasicGraph;
  txt: string;
  path: TList;
  index: integer;
  length: double;
  vTime: TDateTime;
  time, timeG: string;
begin
  try
    vTime := Now();

    graph := GetGraph();

    timeG := IntToStr( DiffDateTimeMsec(Now(), vTime) );

    path := TList.Create();

    vTime := Now();

    length := graph.FindPath( graph.GetNode_ByUID( 2968044917690250096 ), graph.GetNode_ByUID( 2968045942381045440 ), path );

    time := IntToStr( DiffDateTimeMsec(Now(), vTime) );
    
    txt := 'Start = ' + IntToStr( TBasicGraphEdge( path[ 0 ] ).NodeFrom.UID );
    for index := 0 to path.Count - 1 do
      txt := txt + ' ' + IntToStr( TBasicGraphEdge( path[ index ] ).NodeTo.UID );
    txt := txt + ' = End';
    txt := txt + sLineBreak + 'Length: ' + FloatToStr( length );
    txt := txt + sLineBreak + 'Time: ' + time + ' ms';
    txt := txt + sLineBreak + 'Graph Load Time: ' + timeG + ' ms';
    ShowMessage( txt );
  finally
    FreeAndNil( graph );
    FreeAndNil( path );
  end;
end;

end.
