unit basicGraph;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs;

type
  EBasicGraphError = class(Exception);

  TBasicGraphEdgeDirection = ( bgedTwoOne = -1, bgedUndirected = 0, bgedOneTwo = 1 );

  TBasicGraphEdge = class;

  TBasicGraphNode = class( TObject )
  private
    FUniqueID: Integer;
    FObject: TObject;
    FflOwnsObject: Boolean;
    FEdgesList: TList;

  public
    constructor Create( uid: Integer = -1; assignedObject: TObject = nil; flOwnsObject: Boolean = False;
                        edges: TList  = nil );

    //add edge to edge list
    function AddEdge( edge: TBasicGraphEdge ): integer;

    //delete edge from edge list
    procedure DeleteEdge( edge: TBasicGraphEdge );

    //delete edge from edge list by edge's index
    procedure DeleteEdge_ByIndex( index: integer );

    //delete edge from edge list by edge's uid
    procedure DeleteEdge_ByUniqueID( uid: integer );

    //outputs full list of neighbors
    function NeighborList(): TList;

    //��������� ����� �� ���������. ���� �� ����� - false, ����� true
    //������ ��� ������������
    function NodesMatch( node: TBasicGraphNode ): Boolean;

    procedure SetUniqueID( uid: integer );
    procedure SetObject( associatedObject: TObject );
    procedure SetFlOwnsObject( flOwnsObject: boolean );
    procedure SetEdgesList( edgesList: TList );

    property UniqueID: Integer read FUniqueID write SetUniqueID;
    property AssociatedObject: TObject read FObject write SetObject;
    property flOwnsObject: Boolean read FflOwnsObject write SetFlOwnsObject;
    property EdgesList: TList read FEdgesList write SetEdgesList;
  end;

  TBasicGraphEdge = class( TObject )
  private
    FUniqueID: integer;
    FObject: TObject;
    FflOwnsObject: Boolean;
    FNode1: TBasicGraphNode;
    FNode2: TBasicGraphNode;
    FWeight: Double;
    //direction
    // -1 : 2 -> 1 - bgedTwoOne
    // 0 : not directed - bgedUndirected
    // 1 : 1 -> 2 - bgedOneTwo
    FDirection: TBasicGraphEdgeDirection;

  public
    constructor Create( node1, node2: TBasicGraphNode; uid: integer = -1; associatedObject: TObject = nil;
                        flOwnsObject: Boolean = false; weight: Double = 1.0;
                        direction: TBasicGraphEdgeDirection = bgedUndirected );

    //�������� ������ ����, ������� �� ���� ����
    function GetOtherNode( node: TBasicGraphNode ): TBasicGraphNode;

    procedure SetUniqueID( uid: integer );
    procedure SetObject( AssociatedObject: TObject );
    procedure SetFlOwnsObject( flOwnsObject: boolean );
    procedure SetNode1( new_node1: TBasicGraphNode );
    procedure SetNode2( new_node2: TBasicGraphNode );
    procedure SetDirection( direction: TBasicGraphEdgeDirection );
    procedure SetWeight( weight: double );

    function EdgesMatch( edge: TBasicGraphEdge ): boolean;

    property UniqueID: Integer read FUniqueID write SetUniqueID;
    property AssociatedObject: TObject read FObject write SetObject;
    property flOwnsObject: Boolean read FflOwnsObject write SetFlOwnsObject;
    property Node1: TBasicGraphNode read FNode1 write SetNode1;
    property Node2: TBasicGraphNode read FNode2 write SetNode2;
    property Direction: TBasicGraphEdgeDirection read FDirection write SetDirection;
    property Weight: Double read FWeight write SetWeight;
  end;

  TBasicGraph = class( TObject )
  private
    FNodes: TList;
    FEdges: TList;

  public
    constructor Create( nodesList: TList = nil; edgesList: TList = nil );

    //node procedures and functions
    //�������� ���� � ������ �����, ���������� ������
    function AddNode( node: TBasicGraphNode ): integer;
    //������ � ��������� ���� � ������ �����
    function CreateAndAddNode( uid: Integer = -1; assignedObject: TObject = nil; flOwnsObject: Boolean = False;
                                edges: TList  = nil ): integer;
    //������� ���� �� ������
    procedure DeleteNode( node: TBasicGraphNode );
    //������� ���� �� ������, ����� ��� �� �������
    procedure DeleteNode_ByIndex( index: Integer );
    //������� ���� �� ������, ����� ��� �� Unique ID
    procedure DeleteNode_ByUniqueID( uid: Integer );

    //����� ���� � ������, ������� ������ ��� -1, ���� ������ ���
    function FindNode( node: TBasicGraphNode ): Integer;
    // ����� ���� � ������, ������� ������ ��� -1, ���� ������ ���
    function FindNode_ByUniqueID( uid: integer ): integer;

    //edge procedures and functions
    //�������� ���� � ������ ���, ���������� ������
    function AddEdge( edge: TBasicGraphEdge ): Integer;
    //������� � �������� ����
    function CreateAndAddEdge( node1, node2: TBasicGraphNode; uid: integer = -1; associatedObject: TObject = nil;
                                flOwnsObject: Boolean = false; weight: Double = 1.0;
                                direction: TBasicGraphEdgeDirection = bgedUndirected ): integer;
    //������� ���� �� ������ ���
    procedure DeleteEdge( edge: TBasicGraphEdge );
    //������� ���� �� ������, ����� �� �������
    procedure DeleteEdge_ByIndex( index: Integer );
    //������� ���� �� ������, ����� �� Unique ID
    procedure DeleteEdge_ByUniqueID( uid: integer );

    //����� ���� � ������ ���, ������� ������ ��� -1
    function FindEdge( edge: TBasicGraphEdge ): Integer;
    //����� ���� � ������ ��� �� �������, ������� ������ ��� -1
    function FindEdge_ByUniqueID( uid: integer ): integer;

    //�������
    procedure SetNodes( nodesList: Tlist );
    procedure SetEdges( edgesList: Tlist );

    property Nodes: TList read FNodes write SetNodes;
    property Edges: Tlist read FEdges write SetEdges;
  end;

implementation

{**********************************************************************************************
* TBasicGraphNode.SetUniqueID
***********************************************************************************************}
procedure TBasicGraphNode.SetUniqueID( uid: integer );
begin
  Self.FUniqueID := uid;
end;

{**********************************************************************************************
* TBasicGraphNode.SetObject
***********************************************************************************************}
procedure TBasicGraphNode.SetObject( associatedObject: TObject );
begin
  Self.FObject := associatedObject;
end;

{**********************************************************************************************
* TBasicGraphNode.SetFlOwnsObject
***********************************************************************************************}
procedure TBasicGraphNode.SetFlOwnsObject( flOwnsObject: boolean );
begin
  self.FflOwnsObject := flOwnsObject;
end;

{**********************************************************************************************
* TBasicGraphNode.SetEdgesList
***********************************************************************************************}
procedure TBasicGraphNode.SetEdgesList( edgesList: TList );
begin
  self.FEdgesList.Assign( edgesList );
end;

{**********************************************************************************************
* TBasicGraphNode.Create
***********************************************************************************************}
constructor TBasicGraphNode.Create( uid: Integer = -1; assignedObject: TObject = nil; flOwnsObject: Boolean = False;
                                     edges: TList  = nil );
var
  index: integer;
begin
  inherited Create();

  FUniqueID := uid;
  FObject := assignedObject;
  FflOwnsObject := flOwnsObject;

  //���� ������ ��� �� ������
  if edges <> nil then
  begin
    //��������, ��� �� �� �������� �������� � ������ ��� ���� ���, �� ��������� � �����
    for index := 0 to edges.Count - 1 do
       if ( TBasicGraphEdge( edges[index] ).Node1 <> Self )
        AND ( TBasicGraphEdge( edges[index] ).Node2 <> Self )
       then
        raise EBasicGraphError.Create( '� ������ ��� ���� ����������� ����, �� ��������� � ����� �����. Ÿ Unique ID: '
                                        + IntToStr( TBasicGraphEdge( edges[index] ).UniqueID ) );

    FEdgesList.Assign( edges );
  end
  else
    FEdgesList := TList.Create();
end;

{**********************************************************************************************
* TBasicGraphNode.AddEdge
* add edge to edge list of node
* returns index of added element
***********************************************************************************************}
function TBasicGraphNode.AddEdge( edge: TBasicGraphEdge ): integer;
var
  index: integer;
begin
  Result := -1;

  if edge = nil then
    raise EBasicGraphError.Create( '���������� �������� ���� � ������ ��� ����, �.�. ���� ������� (nil).' );

  if FEdgesList = nil then
    raise EBasicGraphError.Create( '������� ������ ���, � ������� ������ ���� (nil).' );

  try
    for index := 0 to FEdgesList.Count - 1 do
    //���� ���� ����� � ����� UID, �� ������ �����
    if TBasicGraphEdge( FEdgesList[ index ] ) = edge then
      Exit;

    Result := FEdgesList.Add( edge );
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� �������� ���� � ������ ���.' + sLineBreak + '��������� �� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge( edge: TBasicGraphEdge );
var
  index: integer;
begin
  if edge = nil then
    raise EBasicGraphError.Create( '���������� ������� ����, ��������� ��� ������� (nil).' );

  if FEdgesList = nil then
    raise EBasicGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
  try
    index := FEdgesList.IndexOf( edge );
    if index >= 0 then
      FEdgesList.Delete(index);
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� �� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.
* delete edge from edge list by edge's index
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByIndex( index: integer );
begin    
  if FEdgesList = nil then
    raise EBasicGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
  try
    if ( index >= 0 ) and ( index < FEdgesList.Count ) then
      FEdgesList.Delete(index);
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge_ByUniqueID
* �������� ���������, ������ ��� � ������ ���� �����
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByUniqueID( uid: integer );
var
  index: integer;

  function IndexOf_ByUniqueID( EdgesList: TList; uid: integer ): integer;
  begin
    Result := 0;
    //���� ��� ���������, ������� -1
    if EdgesList.Count = 0 then
    begin
      Result := -1;
      Exit;
    end;

    while ( Result < EdgesList.Count ) and ( TBasicGraphNode( EdgesList[Result] ).UniqueID <> uid ) do
      Inc( Result );

    //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
    if EdgesList.Count = Result then
      Result := -1;
  end;

begin       
  if FEdgesList = nil then
    raise EBasicGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
    
  try
    index := IndexOf_ByUniqueID( FEdgesList, uid );
    if index >= 0 then
      FEdgesList.Delete( index );
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.NeighborList
* outputs full list of neighbors
***********************************************************************************************}
function TBasicGraphNode.NeighborList(): TList;
var
  retval: TList;
  index: integer;
  neighbor: TBasicGraphNode;
begin
  Result := nil;
  if FEdgesList = nil then
    Exit;

  try
    retval := TList.Create();

    for index := 0 to Self.EdgesList.Count - 1 do
    begin
      neighbor := TBasicGraphEdge( Self.EdgesList[ index ] ).GetOtherNode( Self );

      if neighbor = nil then
      begin
        retval.Clear();
        FreeAndNil(retval);
        raise EBasicGraphError.Create( '���������� ���� ������ � ����� �����. Unique ID ���� = ' +
                                        IntToStr( TBasicGraphEdge( Self.EdgesList[ index ] ).UniqueID ) );
      end;

      retval.Add( neighbor );
    end;
  except
    on E: Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ������ ���� ������� ����.' + sLineBreak + '��������� ������:'
                                      + sLineBreak + E.Message );
  end;

  Result := retval;
end;

{**********************************************************************************************
* TBasicGraphNode.NodesMatch
* ��������� ����� �� ���������. ���� �� ����� - false, ����� true
* ������ ��� ������������
***********************************************************************************************}
function TBasicGraphNode.NodesMatch( node: TBasicGraphNode ): Boolean;
var
  index: integer;
  selfEdges, nodeEdges: TList;
begin
  Result := false;

  if self.UniqueID <> node.UniqueID then
    Exit;

  if self.AssociatedObject <> node.AssociatedObject then
    exit;

  if self.flOwnsObject <> node.flOwnsObject then
    Exit;

  if Self.EdgesList.Count <> node.EdgesList.Count then
    exit;

  index := 0;
  selfEdges := Self.EdgesList;
  nodeEdges := node.EdgesList;
  while ( index < selfEdges.Count ) and ( index < nodeEdges.Count ) do
  begin
    if selfEdges[ index ] <> nodeEdges[ index ] then
      Exit;
    index := index + 1;
  end;

  Result := true;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetUniqueID
***********************************************************************************************}
procedure TBasicGraphEdge.SetUniqueID( uid: integer );
begin
  Self.FUniqueID := uid;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetObject
***********************************************************************************************}
procedure TBasicGraphEdge.SetObject( AssociatedObject: TObject );
begin
  Self.FObject := AssociatedObject;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetFlOwnsObject
***********************************************************************************************}
procedure TBasicGraphEdge.SetFlOwnsObject( flOwnsObject: boolean );
begin
  Self.FflOwnsObject := flOwnsObject;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode1
***********************************************************************************************}
procedure TBasicGraphEdge.SetNode1( new_node1: TBasicGraphNode );
begin
  Self.FNode1 := new_node1;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode2
***********************************************************************************************}
procedure TBasicGraphEdge.SetNode2( new_node2: TBasicGraphNode );
begin
  Self.FNode2 := new_node2;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetDirection
***********************************************************************************************}
procedure TBasicGraphEdge.SetDirection( direction: TBasicGraphEdgeDirection );
begin
  Self.FDirection := direction;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetWeight
***********************************************************************************************}
procedure TBasicGraphEdge.SetWeight( weight: double );
begin
  Self.FWeight := weight;
end;

{**********************************************************************************************
* TBasicGraphEdge.Create
***********************************************************************************************}
constructor TBasicGraphEdge.Create( node1, node2: TBasicGraphNode; uid: integer = -1; associatedObject: TObject = nil;
                                    flOwnsObject: Boolean = false; weight: Double = 1.0;
                                    direction: TBasicGraphEdgeDirection = bgedUndirected );
begin
  inherited Create();

  if (node1 = nil) or (node2 = nil) then
    raise EBasicGraphError.Create( '��� ���� ���� ������ ������������ (�� nil).' );

  FUniqueID := uid;
  FObject := associatedObject;
  FflOwnsObject := flOwnsObject;
  FNode1 := node1;
  FNode2 := node2;
  FWeight := weight;
  FDirection := direction;

  //������� ����� ������� ����, ����� ���� ����� ����
  if FNode1.EdgesList.IndexOf( self ) = -1 then
    Fnode1.AddEdge( self );
  if FNode2.EdgesList.IndexOf( self ) = -1 then
    Fnode2.AddEdge( self );
end;

{**********************************************************************************************
* TBasicGraphEdge.GetOtherNode
***********************************************************************************************}
function TBasicGraphEdge.GetOtherNode( node: TBasicGraphNode ): TBasicGraphNode;
begin
  Result := nil;

  if self.FNode1.NodesMatch( node ) then
    Result := self.FNode2
  else if Self.FNode2.NodesMatch( node ) then
    Result := self.FNode1;
end; 

{**********************************************************************************************
* TBasicGraphEdge.EdgesMatch
* match = true
* else = false
***********************************************************************************************}
function TBasicGraphEdge.EdgesMatch( edge: TBasicGraphEdge ): boolean;
begin
  Result := false;

  if Self.UniqueID <> edge.UniqueID then
    Exit;

  if Self.AssociatedObject <> edge.AssociatedObject then
    exit;

  if Self.flOwnsObject <> edge.flOwnsObject then
    exit;

  if Self.Node1 <> edge.Node1 then
    exit;

  if Self.Node2 <> edge.Node2 then
    exit;

  if Self.Weight <> edge.Weight then
    exit;

  if Self.Direction <> edge.Direction then
    exit;

  Result := true;
end;

{**********************************************************************************************
* TBasicGraph.SetNodes
***********************************************************************************************}
procedure TBasicGraph.SetNodes( nodesList: Tlist );
begin
  nodesList.Assign( nodesList );
end;

{**********************************************************************************************
* TBasicGraph.SetEdges
***********************************************************************************************}
procedure TBasicGraph.SetEdges( edgesList: Tlist );
begin
  edgesList.Assign( edgesList );
end;

{**********************************************************************************************
* TBasicGraph.Create
* nil, nil - ������ ���������������� ����� ����� � ���
* nil, smth - �� ������ ��� (� ������ ���� ����������� ���� 2 ����) ���������� ������ �����
* smth, nil - ������ ������������ ������ ����� � ���������������� ������ ���
* smth, smth - ����������� ������� ����� � ����� �� ������ ��� � ������ �����
*   ���� �������������� ����, �������� ��� � ������ �����, ����������� ����������
***********************************************************************************************}
constructor TBasicGraph.Create( nodesList: TList = nil; edgesList: TList = nil );
var
  index: integer;
begin
  try

    if nodesList = nil then
    begin
      if edgesList = nil then
      begin
        //��� ����� �����
        FNodes := TList.Create();
        FEdges := TList.Create();
        exit;
      end
      else
      begin
        //���� �������� ������ ���, � ������ ����� ������
        //����� �������� ������ ����� �� ��������� ������ ���,
        //��������� ���� ������� ���� ���������� ����� ������
        FNodes := TList.Create();
        FEdges.Assign( edgesList );
        for index := 0 to edgesList.Count - 1 do
        begin
          Self.AddNode( TBasicGraphEdge( edgesList[ index ] ).FNode1 );
          Self.AddNode( TBasicGraphEdge( edgesList[ index ] ).FNode2 ); 
        end;
      end;
    end
    else
    begin
      if edgesList = nil then
      begin
        //�������� ������ �����, ������ ������ ���, ������ ������� ���� � FNodes
        FNodes.Assign( nodesList );
        FEdges := TList.Create();
      end
      else
      begin
        //��� ������ �� �����
        //�����������, ���� �� � Nodes ��� ���� �� ��� ��  Edges. ���� ���, �� ����������� ����������
        for index := 0 to edgesList.Count - 1 do
        begin
          if ( Self.FindNode( TBasicGraphEdge( edgesList[ index ] ).FNode1 ) = -1 )
            or ( Self.FindNode( TBasicGraphEdge( edgesList[ index ] ).FNode2 ) = -1 )
          then
            raise EBasicGraphError.Create( '� ������ ��� ������ ����, ������������� � ������ �����. Unique ID ����: ' +
              IntToStr( TBasicGraphEdge( edgesList[ index ] ).FUniqueID ) );

          FNodes.Assign( nodesList );
          FEdges.Assign( edgesList );
        end;
      end;
    end;

  except
    on E: Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.AddNode
* �������� ���� � ������ �����, ���������� ������
***********************************************************************************************}
function TBasicGraph.AddNode( node: TBasicGraphNode ): integer;
begin
  if node = nil then
    raise EBasicGraphError.Create( '������ �������� ������ ���� (nil).' );

  if FNodes = nil then
    raise EBasicGraphError.Create( '������ �������� ����, �.�. �� ��� ��������������� ������ �����.' );

  Result := Self.FindNode( node );
  if Result <> -1 then
    exit;

  Result := FNodes.Add( node );
end;

{**********************************************************************************************
* TBasicGraph.CreateAndAddNode
* ������ � ��������� ���� � ������ �����
***********************************************************************************************}
function TBasicGraph.CreateAndAddNode( uid: Integer = -1; assignedObject: TObject = nil; flOwnsObject: Boolean = False;
                            edges: TList  = nil ): integer;
var
  node: TBasicGraphNode;
begin
  node := TBasicGraphNode.Create( uid, assignedObject, flOwnsObject, edges );

  Result := Self.FindNode( node );
  if Result <> -1 then
  begin
    FreeAndNil( node );
    Exit;
  end;

  Result := FNodes.Add( node );
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode
***********************************************************************************************}
procedure TBasicGraph.DeleteNode( node: TBasicGraphNode );
begin
  //��� ��������� ����, �� ������� ���� ������� � ����, � ������� �� ������
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByIndex
***********************************************************************************************}
procedure TBasicGraph.DeleteNode_ByIndex( index: Integer );
begin
  //��� ��������� ����, �� ������� ���� ������� � ����, � ������� �� ������
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByUniqueID
***********************************************************************************************}
procedure TBasicGraph.DeleteNode_ByUniqueID( uid: Integer );
begin
  //��� ��������� ����, �� ������� ���� ������� � ����, � ������� �� ������
end;

{**********************************************************************************************
* TBasicGraph.FindNode
***********************************************************************************************}
function TBasicGraph.FindNode( node: TBasicGraphNode ): Integer;
var
  index: integer;
begin
  Result := -1;
  for index := 0 to FNodes.Count - 1 do
    if node.NodesMatch( TBasicGraphNode( FNodes[ index ] ) ) then
    begin
      Result := index;
      Exit;
    end;
end;

{**********************************************************************************************
* TBasicGraph.FindNode_ByUniqueID
***********************************************************************************************}
function TBasicGraph.FindNode_ByUniqueID( uid: Integer ): Integer;
var
  index: Integer;
begin
  Result := -1;
  for index := 0 to FNodes.Count - 1 do
    if uid = TBasicGraphNode( FNodes[ index ] ).UniqueID then
    begin
      Result := index;
      Exit;
    end;
end;

{**********************************************************************************************
*  TBasicGraph.AddEdge
***********************************************************************************************}
function TBasicGraph.AddEdge( edge: TBasicGraphEdge ): integer;
begin
  if edge = nil then
    raise EBasicGraphError.Create( '������ �������� ������ ���� (nil).' );

  if FEdges = nil then
    raise EBasicGraphError.Create( '������ �������� ����, �.�. ������ ��� �� ��� ���������������.' );

  Result := Self.FindEdge( edge );
  if Result <> -1 then
    exit;

  Result := FEdges.Add( edge );
end;

{**********************************************************************************************
* TBasicGraph.CreateAndAddEdge
***********************************************************************************************}
function TBasicGraph.CreateAndAddEdge( node1, node2: TBasicGraphNode; uid: integer = -1; associatedObject: TObject = nil;
                                        flOwnsObject: Boolean = false; weight: Double = 1.0;
                                        direction: TBasicGraphEdgeDirection = bgedUndirected ): integer;
var
  edge: TBasicGraphEdge;
begin
  edge := TBasicGraphEdge.Create( node1, node2, uid, associatedObject, flOwnsObject, weight, direction );

  Result := Self.FindEdge( edge );
  if Result <> -1 then
  begin
    FreeAndNil( edge );
    Exit;
  end;

  Result := FEdges.Add( edge );
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge
* ������� ���� �� ������ ���
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge( edge: TBasicGraphEdge );
var
  index: integer;
begin
  if edge = nil then
    exit;

  if FEdges = nil then
    raise EBasicGraphError.Create( '������ ������� ���� �� �����, �.�. �� ��������������� ������ ���.' );

  try
    index := FEdges.IndexOf( edge );
    if index >= 0 then
      FEdges.Delete(index);
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� �� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
*  TBasicGraph.DeleteEdge_ByIndex
* ������� ���� �� ������, ����� �� �������
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByIndex( index: Integer );
begin    
  if FEdges = nil then
    raise EBasicGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
  try
    if ( index >= 0 ) and ( index < FEdges.Count ) then
      FEdges.Delete(index);
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge_ByUniqueID
* ������� ���� �� ������, ����� �� Unique ID
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByUniqueID( uid: integer );
var
  index: integer;

  function IndexOf_ByUniqueID( EdgesList: TList; uid: integer ): integer;
  begin
    Result := 0;
    //���� ��� ���������, ������� -1
    if EdgesList.Count = 0 then
    begin
      Result := -1;
      Exit;
    end;

    while ( Result < EdgesList.Count ) and ( TBasicGraphNode( EdgesList[Result] ).UniqueID <> uid ) do
      Inc( Result );

    //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
    if EdgesList.Count = Result then
      Result := -1;
  end;

begin       
  if FEdges = nil then
    raise EBasicGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
  try
    index := IndexOf_ByUniqueID( self.Edges, uid );
    if index >= 0 then
      FEdges.Delete( index );
  except
    on E:Exception do
      raise EBasicGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.FindEdge
***********************************************************************************************}
function TBasicGraph.FindEdge( edge: TBasicGraphEdge ): integer; 
var
  index: integer;
begin
  Result := -1;
  
  if edge = nil then
    raise EBasicGraphError.Create( '���������� ���������� ������ ���� (nil).' );

  if FEdges = nil then
    raise EBasicGraphError.Create( '���������� ����� ����, �.�. ���� ������ ��� (nil).' );

  for index := 0 to FEdges.Count - 1 do
    if edge.EdgesMatch( TBasicGraphEdge( FEdges[ index ] ) ) then
    begin
      Result := index;
      Exit;
    end;
end;

{**********************************************************************************************
*TBasicGraph.FindEdge_ByUniqueID
***********************************************************************************************}
function TBasicGraph.FindEdge_ByUniqueID( uid: integer ): Integer;
var
  index: Integer;
begin
  Result := -1;
  for index := 0 to FEdges.Count - 1 do
    if uid = TBasicGraphEdge( FEdges[ index ] ).UniqueID then
    begin
      Result := index;
      Exit;
    end;
end;

end.
