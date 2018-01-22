unit testGraphForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs,
  basicGraph;

type
  TForm1 = class(TForm)
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

procedure TForm1.FormCreate(Sender: TObject);
var
  node: TBasicGraphNode;
  edge: TBasicGraphEdge;
  nodesList, edgesList, neighborList: TList;
  index: integer;
begin
  nodesList := TList.Create();
  nodesList.Add( TBasicGraphNode.Create( 1, nil, false, nil ) );
  nodesList.Add( TBasicGraphNode.Create( 2, nil, false, nil ) );
  nodesList.Add( TBasicGraphNode.Create( 3, nil, false, nil ) );
  nodesList.Add( TBasicGraphNode.Create( 4, nil, false, nil ) );

  edgesList := TList.Create();
  edgesList.Add( TBasicGraphEdge.Create( nodesList[0], nodesList[1], 1, nil, false , 1, bgedOneTwo ) );
  edgesList.Add( TBasicGraphEdge.Create( nodesList[1], nodesList[2], 1, nil, false , 1, bgedOneTwo ) );
  edgesList.Add( TBasicGraphEdge.Create( nodesList[2], nodesList[3], 1, nil, false , 1, bgedOneTwo ) );

  TBasicGraphNode( nodesList[0] ).DeleteEdge_ByIndex(0);
  neighborList := TBasicGraphNode( nodesList[0] ).NeighborList();
  ShowMessage( IntToStr( neighborList.Count ) );
  FreeAndNil( neighborList );

  for index := 0 to nodesList.Count - 1 do
  begin
    node := TBasicGraphNode( nodesList[index] );
    FreeAndNil( node );
  end;
  FreeAndNil( nodesList );
  for index := 0 to edgesList.Count - 1 do
  begin
    edge := TBasicGraphEdge( edgesList[index] );
    FreeAndNil( edge );
  end;
  FreeAndNil( edgesList );
end;

end.
