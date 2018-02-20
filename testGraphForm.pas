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
  path: TList;
  index: integer;
  length: double;
begin
  //
  graph := TBasicGraph.Create();
  
  try
    obj := TObject.Create();
    graph.AddNode( 1 );
    graph.AddNode( 2, obj );
    graph.AddNode( 3 );
    graph.AddNode( 4 );

    graph.AddEdge( graph.GetNode_ByUID( 1 ), graph.GetNode_ByUID( 2 ), 10, 1 );
    graph.AddEdge( graph.GetNode_ByUID( 2 ), graph.GetNode_ByUID( 3 ), 20, 2 );
    //graph.AddEdge( graph.GetNode_ByUID( 3 ), graph.GetNode_ByUID( 4 ), 30, 3 );
    graph.AddEdge( graph.GetNode_ByUID( 1 ), graph.GetNode_ByUID( 4 ), 40, 7.9 );
    graph.AddEdge( graph.GetNode_ByUID( 4 ), graph.GetNode_ByUID( 3 ), 50, 1 );
    graph.AddEdge( graph.GetNode_ByUID( 2 ), graph.GetNode_ByUID( 1 ), 60, 10 );

    path := TList.Create();
    length := graph.FindPath( graph.GetNode_ByUID( 2 ), graph.GetNode_ByUID( 4 ), path );
    txt := '';
    for index := 0 to path.Count - 1 do
      txt := txt + ' ' + IntToStr( TBasicGraphNode( path[ index ] ).UID );

    ShowMessage( txt + ' Len: ' + FloatToStr( length ) );
  finally
    FreeAndNil( graph );
    FreeAndNil( obj );
    FreeAndNil( path );
  end;
end;

end.
