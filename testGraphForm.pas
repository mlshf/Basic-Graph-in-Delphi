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
  graph: TBasicGraph;
  txt: string;
  node: TBasicGraphNode;
  edge: TBasicGraphEdge;
  obj: TObject;
begin
  //
  graph := TBasicGraph.Create();
  
  try
    obj := TObject.Create();
    graph.AddNode( 1 );
    graph.AddNode( 2 );

    graph.AddEdge( graph.GetNode_ByUID( 1 ), graph.GetNode_ByUID( 2 ), 1);

    ShowMessage( IntToStr( graph.GetNode_ByUID( 1 ).GetNeighbour_ByIndex( 0, bgdFrom ).GetNeighboursCount( bgdTo ) ) );
  finally
    FreeAndNil(graph);
    FreeAndNil(obj);
  end;

end;

end.
