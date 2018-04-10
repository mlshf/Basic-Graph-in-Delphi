unit basicGraph;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uniTypes, math, mapplMath, Contnrs, uniHashContainer, maps, mapsSys,
  uniBaseTypes;

type
  EGraphError = class(Exception);

  TGraphDirection = ( gdFrom = 0, gdTo = 1 );

  TGraphDirectionSet = set of TGraphDirection;

  TGraphDeletionMode = ( gdmAllowOnlyTarget = 0, gdmAllowTargetAndEdges = 1 );

  TBasicGraphEdge = class;

  TEdgeStruct = class
  protected
    FEdge: TBasicGraphEdge;
    FCost: Double;
  public

    constructor Create( AEdge: TBasicGraphEdge; ACost: Double ); reintroduce;

    property Edge: TBasicGraphEdge read FEdge write FEdge;
    property Cost: Double read FCost write FCost;
  end;

  TEdgesQueueWithPriority = class
  protected
    FQueue: TList;
  public
    constructor Create();
    function IsEmpty(): Boolean;
    function GetElement(): TEdgeStruct;
    //�������� ������� � ������������ ������
    procedure AddElement( AEdgeRecord: TEdgeStruct );
    destructor Destroy; override;
  end;

  //������� ����� - �������� ���� �����
  TBasicGraphNode = class( TObject )
  protected
    FIndex: Integer;
    //���������� �������������
    FUID: Int64;
    //��������������� � ����� ������
    FObject: TObject;
    //���� �������� ������� ��� �������� ����
    FflOwnsObject: Boolean;
    //������ ���, �������� � ����
    FEdgesList_To: TList;
    //������ ���, ��������� �� ����
    FEdgesList_From: TList;
    //������ �����-�������, �� ������� ����� ������� � ����
    FNeighboursList_To: TList;
    //������ �����-�������, � ������� ����� ������� �� ����
    FNeighboursList_From: TList;

    //�������� ���� � ������ (��� � ��� ������) ���
    procedure AddEdge( AEdge: TBasicGraphEdge );

    //��������������� ��������� ��� �������� ���� � ��������� � ��� �������
    //������� �������� ���� � � ���� From, ���� �����
    procedure DeleteEdgeAndNeighbour_To( index: Integer; NodeFrom: TBasicGraphNode );
    //������� �s������� ���� � � ���� To, ���� �����
    procedure DeleteEdgeAndNeighbour_From( index: Integer; NodeTo: TBasicGraphNode );
    //������� ���� �� ����� ������� ���
    procedure DeleteEdge( AEdge: TBasicGraphEdge );
    //������� ���� �� ����� ������� ���, ����� � �� UID
    procedure DeleteEdge_ByUID( AUID: Int64 );
    //������� ���� �� ����� ������� ���, ����� � �� �������
    procedure DeleteEdge_ByObject( AObject: TObject );

    //������ ����-������ � ������ ������� To/From �� UID
    function IndexOfNeighbour_ByUID( AListDirection: TGraphDirection; AUID: Int64 ): Integer;
    //������ ����-������ � ������ ������� To/From �� �������
    function IndexOfNeighbour_ByObject( AListDirection: TGraphDirection; AObject: TObject ): Integer;

  public
    constructor Create( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False );

    destructor Destroy(); override;

    //----------------------------------------------------------------------------------------------------------------

    //�������� ������� ����-������
    //��������� ������� ����-������ � ������� ������� From/To/From&To
    function HasNeighbour( ANode: TBasicGraphNode; AListDirections: TGraphDirectionSet ): Boolean;
    //��������� ������� ����-������ � ������� ������� From/To/From&To �� UID
    function HasNeighbour_ByUID( AUID: Int64; AListDirections: TGraphDirectionSet ): Boolean;
    //��������� ������� ����-������ � ������� ������� From/To/From&To �� ���������������� �������
    function HasNeighbour_ByObject( AObject: TObject; AListDirections: TGraphDirectionSet ): Boolean;

    //������ � ������-��������
    //���������� ���������� �����-������� � ������ From/To
    function GetNeighboursCount( AListDirection: TGraphDirection ): Integer;
    //���������� ���� �� � ������� � ������ From/To
    function GetNeighbour_ByIndex( AIndex: Integer; AListDirection: TGraphDirection ): TBasicGraphNode;
    //���������� ���� �� � UID, ���� � ������� From/To/From&To
    function GetNeighbour_ByUID( AUID: Int64; AListDirection: TGraphDirection ): TBasicGraphNode;
    //���������� ���� �� ���������������� � ��� �������, ���� � ������� From/To/From&To
    function GetNeighbour_ByObject( AObject: TObject; AListDirection: TGraphDirection ): TBasicGraphNode;

    //----------------------------------------------------------------------------------------------------------------

    //������ ���� � ������ ��� To/From �� UID
    function IndexOfEdge_ByUID( AListDirection: TGraphDirection; AUID: Int64 ): Integer;
    //������ ���� � ������ ��� To/From �� �������
    function IndexOfEdge_ByObject( AListDirection: TGraphDirection; AObject: TObject ): Integer;

    //�������� ������� ����
    //��������� ������� ���� � ������� ��� From/To/From&To
    function HasEdge( AEdge: TBasicGraphEdge; AListDirections: TGraphDirectionSet ): Boolean;
    //��������� ������� ���� � ������� ��� From/To/From&To �� UID
    function HasEdge_ByUID( AUID: Int64; AListDirections: TGraphDirectionSet ): Boolean;
    //��������� ������� ���� � ������� ��� From/To/From&To �� ���������������� �������
    function HasEdge_ByObject( AObject: TObject; AListDirections: TGraphDirectionSet ): Boolean;

    //������ � ������
    //���������� ���������� ��� � ������ From/To
    function GetEdgesCount( AListDirection: TGraphDirection ): Integer;
    //���������� ���� �� � ������� � ������ From/To
    function GetEdge_ByIndex( AIndex: Integer; AListDirection: TGraphDirection ): TBasicGraphEdge;
    //���������� ���� �� � UID, ���� � ������� From/To/From&To
    function GetEdge_ByUID( AUID: Int64; AListDirection: TGraphDirection ): TBasicGraphEdge;
    //���������� ���� �� ���������������� � ��� �������, ���� � ������� From/To/From&To
    function GetEdge_ByObject( AObject: TObject; AListDirection: TGraphDirection ): TBasicGraphEdge;

    //----------------------------------------------------------------------------------------------------------------

    //�������
    procedure SetObject( AObject: TObject );
    procedure SetFlOwnsObject( AflOwnsObject: boolean );

    property UID: Int64 read FUID;
    property Object_: TObject read FObject write SetObject;
    property flOwnsObject: Boolean read FflOwnsObject write SetFlOwnsObject;
    property NodeIndex: Integer read FIndex;
  end;

  //������� ����� - �������� ���� �����
  TBasicGraphEdge = class( TObject )
  protected
    FIndex: Integer;
    //���������� �������������
    FUID: Int64;
    //��������� � ����� ������
    FObject: TObject;
    //���� �������� ������� ��� �������� ����
    FflOwnsObject: Boolean;
    //���� - ������
    FNodeFrom: TBasicGraphNode;
    //���� - �����
    FNodeTo: TBasicGraphNode;
    //���� ����������������� ����. False (��-���������): ����������� ���� From -> To. True: ����������� ���� From <-> To. 
    FflBiDirected: Boolean;
    //��� ����. ��-��������� ����� 1.
    FWeight: Double;

  public
    constructor Create( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                        AflOwnsObject: Boolean = false; AflBiDirected: Boolean = false;
                        AWeight: Double = 1.0 );

    destructor Destroy(); override;

    //�������
    function GetNodeFrom(): TBasicGraphNode; virtual;
    function GetNodeTo(): TBasicGraphNode; virtual;

    //�������                    
    procedure SetObject( AObject: TObject );
    procedure SetFlOwnsObject( AflOwnsObject: boolean );
    procedure SetNodeFrom( ANewNodeFrom: TBasicGraphNode ); virtual;
    procedure SetNodeTo( ANewNodeTo: TBasicGraphNode ); virtual;
    procedure SetflBiDirected( AflBiDirected: Boolean );
    procedure SetWeight( AWeight: double );

    property UID: Int64 read FUID;
    property Object_: TObject read FObject write SetObject;
    property FlOwnsObject: Boolean read FflOwnsObject write SetFlOwnsObject;
    property NodeFrom: TBasicGraphNode read GetNodeFrom write SetNodeFrom;
    property NodeTo: TBasicGraphNode read GetNodeTo write SetNodeTo;
    property FlBiDirected: Boolean read FflBiDirected write FflBiDirected;
    property Weight: Double read FWeight write SetWeight;
    property EdgeIndex: Integer read FIndex;
  end;

  TBasicGraph = class( TObject )
  protected
    FHighestNodeIndexValue: Integer;
    FHIghestEdgeIndexValue: Integer;
    //���-���������, �������� ���� �� UID. �������� ������ ���������.
    FNodes_ByUID: THashContainer;
    //���-���������, �������� ���� �� ���������������� �������. �� �������� ������ ���������.
    FNodes_ByObject: THashContainer;
    //���-���������, �������� ���� �� UID. �������� ������ ���������.
    FEdges_ByUID: THashContainer;
    //���-���������, �������� ���� �� ���������������� �������. �������� ������ ���������.
    FEdges_ByObject: THashContainer;

    //���������� ����
    function CreateNode( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                          AflOwnsObject: Boolean = False ): TBasicGraphNode; virtual;

    //���������� ����
    function CreateEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1;
                          AObject: TObject = nil; AflOwnsObject: Boolean = false;
                          AflBiDirected: Boolean = false; AWeight: Double = 1.0 ): TBasicGraphEdge; virtual;

  public
    constructor Create();

    destructor Destroy(); override;

    //----------------------------------------------------------------------------------------------------------------

    //�������� ���� � ������ �����, ���������� ������
    function AddNode( AUID: Int64 = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ): TBasicGraphNode;

    //�������� ����
    //������� ����. ���������� True ��� ������� ��������, False ��� ������������� ��������
    function DeleteNode( ANode: TBasicGraphNode;
                          ADeletionMode: TGraphDeletionMode = gdmAllowOnlyTarget ): Boolean;
    //������� ���� �� UID. ���������� True ��� ������� ��������, False ��� ������������� ��������
    function DeleteNode_ByUID( AUID: Int64;
                                    ADeletionMode: TGraphDeletionMode = gdmAllowOnlyTarget ): Boolean;
    //������� ���� �� �������. ���������� True ��� ������� ��������, False ��� ������������� ��������
    function DeleteNode_ByObject( AObject: TObject;
                                  ADeletionMode: TGraphDeletionMode = gdmAllowOnlyTarget ): Boolean;

    //����� ����
    //�������� ���� �� ��� UID
    function GetNode_ByUID( AUID: Int64 ): TBasicGraphNode;
    //��������� ���� �� ��� �������
    function GetNode_ByObject( AObject: TOBject ): TBasicGraphNode; 

    //----------------------------------------------------------------------------------------------------------------

    //���������� ����
    //�������� ���� � ������ ���, ���������� ������
    function AddEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1;
                      AObject: TObject = nil; AflOwnsObject: Boolean = false;
                      AflBiDirected: Boolean = false; AWeight: Double = 1.0 ): TBasicGraphEdge;

    //�������� ����
    //������� ����.
    procedure DeleteEdge( AEdge: TBasicGraphEdge );
    //������� ���� �� UID.
    procedure DeleteEdge_ByUID( AUID: Int64 );
    //������� ���� �� �������.
    procedure DeleteEdge_ByObject( AObject: TObject );

    //����� ����
    //�������� ���� �� � UID
    function GetEdge_ByUID( AUID: Int64 ): TBasicGraphEdge;
    //��������� ���� �� � �������
    function GetEdge_ByObject( AObject: TOBject ): TBasicGraphEdge;

    //------------------------------------------------------------------------------------------------------------------

    //�������� ���� �� ���� (SPLIT)
    function AddNode_ToEdge( AEdge: TBasicGraphEdge; ANode: TBasicGraphNode; AEdge_NewWeight: Double = 1.0;
                              ANewEdge_UID: Int64 = -1; ANewEdge_Object: TObject = nil;
                              ANewEdge_flOwnsObject: Boolean = false ):
                              Boolean;

    //delete and merge �������� ������ ��� ����� � 1 �������� ����� � 1 ��������� �����
    //������� ���� � ���������� �������� � ��������� ���� � ����.
    function DeleteNode_AndMergeEdges( ANode: TBasicGraphNode ): Boolean;
    //������� ���� �� UID � ���������� �������� � ��������� ���� � ����.
    function DeleteNode_AndMergeEdges_ByUID( AUID: Int64 ): Boolean;
    //������� ���� �� ������� � ���������� �������� � ��������� ���� � ����.
    function DeleteNode_AndMergeEdges_ByObject( AObject: TObject ): Boolean;

    //���������� ������ ���������� ��� ����� ����� ��� ��������� ������ ����
    function GetAppropriateEdges_for_FindPath( ANode: TBasicGraphNode ): TList; virtual;

    //����� ���� �� ����� nodeFrom � ����� nodeTo
    function FindPath(  nodeFrom, nodeTo: TBasicGraphNode; path: TList ): double;
  end;

implementation

constructor TEdgeStruct.Create( AEdge: TBasicGraphEdge; ACost: Double );
begin
  FEdge := AEdge;
  FCost := ACost;
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.Create
***********************************************************************************************}
constructor TEdgesQueueWithPriority.Create();
begin
  FQueue := TList.Create();
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.IsEmpty
***********************************************************************************************}
function TEdgesQueueWithPriority.IsEmpty(): Boolean;
begin
  Result := false;
  if ( FQueue.Count = 0 ) then
  begin
    Result := true;
  end;
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.GetElement
***********************************************************************************************}
function TEdgesQueueWithPriority.GetElement(): TEdgeStruct;
begin
  Result := nil;
  if FQueue.Count > 0 then
  begin
    Result := FQueue[ 0 ];
    FQueue.Delete( 0 );
  end;
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.AddElement
* �������� ������� � ������������ ������
***********************************************************************************************}
procedure TEdgesQueueWithPriority.AddElement( AEdgeRecord: TEdgeStruct );
var
  startIndex, endIndex, position: integer;
  edgeRecord: TEdgeStruct;
begin
  if ( FQueue.Count = 0 ) then
  begin
    FQueue.add( AEdgeRecord );
    Exit;
  end;

  // ���� ����� ���������
  edgeRecord:= FQueue[0];
  if ( edgeRecord.Cost >= AEdgeRecord.Cost ) then
  begin
    FQueue.Insert( 0, AEdgeRecord );
    Exit;
  end;

  // ���� 1 ������� � ������
  if ( FQueue.Count = 1 ) then
  begin
    FQueue.add( AEdgeRecord );
    Exit;
  end;

  startIndex := 0;
  endIndex := FQueue.Count - 1;
  while ( ( endIndex - startIndex ) > 1 ) do
  begin
    position := ( endIndex + startIndex ) div 2; //<---
    edgeRecord := FQueue[ position ];
    if ( edgeRecord.Cost = AEdgeRecord.Cost ) then
    begin
      FQueue.Insert( position, AEdgeRecord );
      Exit;
    end;

    if ( edgeRecord.Cost > AEdgeRecord.Cost ) then
      endIndex:= position;
    if ( edgeRecord.Cost < AEdgeRecord.Cost ) then
      startIndex:= position;
  end;
  edgeRecord:= FQueue[ startIndex ];
  if ( edgeRecord.Cost > AEdgeRecord.Cost ) then
    FQueue.Insert( startIndex, AEdgeRecord )
  else
    FQueue.Insert( endIndex, AEdgeRecord );
end;

{**********************************************************************************************
* TEdgesQueueWithPriority.Destroy
***********************************************************************************************}
destructor TEdgesQueueWithPriority.Destroy();
begin
  FreeAndNil( FQueue );
end;

{**********************************************************************************************
* TBasicGraphNode.SetObject
***********************************************************************************************}
procedure TBasicGraphNode.SetObject( AObject: TObject );
begin
  Self.FObject := AObject;
end;

{**********************************************************************************************
* TBasicGraphNode.SetFlOwnsObject
***********************************************************************************************}
procedure TBasicGraphNode.SetFlOwnsObject( AflOwnsObject: boolean );
begin
  self.FflOwnsObject := flOwnsObject;
end;

{**********************************************************************************************
* TBasicGraphNode.Create
***********************************************************************************************}
constructor TBasicGraphNode.Create( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False );
begin
  inherited Create();

  FIndex := AIndex;
  FUID := AUID;
  FObject := AObject;
  FflOwnsObject := AflOwnsObject;

  FEdgesList_To := TList.Create();
  FEdgesList_From := TList.Create();
  FNeighboursList_To := TList.Create();
  FNeighboursList_From := TList.Create();
end;

{**********************************************************************************************
* TBasicGraphNode.Destroy
***********************************************************************************************}
destructor TBasicGraphNode.Destroy();
begin
  FreeAndNil( FEdgesList_To );
  FreeAndNil( FEdgesList_From );
  FreeAndNil( FNeighboursList_To );
  FreeAndNil( FNeighboursList_From );

  if FflOwnsObject then
    FreeAndNil( FObject );

  inherited Destroy();
end;

{**********************************************************************************************
* TBasicGraphNode.AddEdge
* ��������� ���� � ��������������� ������ ���
* ��������� ����-����� � ��������������� ������ �������
***********************************************************************************************}
procedure TBasicGraphNode.AddEdge( AEdge: TBasicGraphEdge );

  //��������� ������ ������ � ������ From
  procedure AddEdge_FromSelf( AEdge: TBasicGraphEdge );
  var
    index: integer;
  begin
    //���� ���� ���� � ������ Edges From, �� ������ �����
    for index := 0 to FEdgesList_From.Count - 1 do
      if TBasicGraphEdge( FEdgesList_From[ index ] ) = AEdge then
        Exit;

    FEdgesList_From.Add( AEdge );

    //���� ���� ����� � ������ Neighbors From, �� ������ �����
    for index := 0 to FNeighboursList_From.Count - 1 do
      if TBasicGraphNode( FNeighboursList_From[ index ] ) = AEdge.NodeTo then
        Exit;
        
    FNeighboursList_From.Add( AEdge.NodeTo );
  end;

  //��������� ������ ������ � ������ To
  procedure AddEdge_ToSelf( AEdge: TBasicGraphEdge );
  var
    index: integer;
  begin
    //���� ���� ���� � ������ Edges To, �� ������ �����
    for index := 0 to FEdgesList_To.Count - 1 do
      if TBasicGraphEdge( FEdgesList_To[ index ] ) = AEdge then
        Exit;
    FEdgesList_To.Add( AEdge );

    //���� ���� ����� � ������ Neighbours To, �� ������ �����
    for index := 0 to FNeighboursList_To.Count - 1 do
      if TBasicGraphNode( FNeighboursList_To[ index ] ) = AEdge.NodeFrom then
        Exit;
    FNeighboursList_To.Add( AEdge.NodeFrom );
  end;

begin
  if AEdge = nil then
    raise EGraphError.Create( '���������� �������� ���� � ������ ��� ����, �.�. ���� ������� (nil).' );

  if ( FEdgesList_To = nil )
    OR ( FEdgesList_From = nil )
    OR ( FNeighboursList_To = nil )
    OR ( FNeighboursList_From = nil ) then
    raise EGraphError.Create( '���� �� ������� ���/������� ������� (nil).' );

  try
    //���� ���� ���������������
    if AEdge.flBiDirected then
    begin
      AddEdge_FromSelf( AEdge );
      AddEdge_ToSelf( AEdge );
    end
    else//���� ����������������
    begin
      if Self = AEdge.NodeFrom then
      begin
        AddEdge_FromSelf( AEdge );
      end
      else
      begin
        if Self = AEdge.NodeTo then
        begin
          AddEdge_ToSelf( AEdge );  
        end
        else
        begin
          raise EGraphError.Create( '������� �������� � ������ ��� ���� ����, �� ��������� � ���� �����!' );
        end;
      end;
    
    end;
  except
    on E:Exception do
      raise EGraphError.Create( '�� ������� �������� ���� � ������ ���.' + sLineBreak + '��������� �� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

procedure TBasicGraphNode.DeleteEdgeAndNeighbour_To( index: Integer; NodeFrom: TBasicGraphNode );
var
  flDeleteNode: boolean;
begin
  if index < 0 then
    Exit;

  FEdgesList_To.Delete(index);
  flDeleteNode := true;

  //������ ���� ���������, �������� �� ���� ������� ����, �� �������� �������� ����
  for index := 0 to FEdgesList_To.Count - 1 do
    if TBasicGraphEdge( FEdgesList_To[ index ] ).NodeFrom = NodeFrom then
    begin
      flDeleteNode := false;
      break;
    end;

  if flDeleteNode then
  begin
    index := FNeighboursList_To.IndexOf( NodeFrom );
    if index >= 0 then
      FNeighboursList_To.Delete( index );
  end;
end;

procedure TBasicGraphNode.DeleteEdgeAndNeighbour_From( index: Integer; NodeTo: TBasicGraphNode );
var
  flDeleteNode: boolean;
begin
  if index < 0 then
    Exit;
    
  FEdgesList_From.Delete(index);
  flDeleteNode := true;

  //������ ���� ���������, �������� �� ���� ������� ����, � ������� ������� ����
  for index := 0 to FEdgesList_From.Count - 1 do
    if TBasicGraphEdge( FEdgesList_From[ index ] ).NodeTo = NodeTo then
    begin
      flDeleteNode := false;
      break;
    end;

  if flDeleteNode then
  begin
    index := FNeighboursList_From.IndexOf( NodeTo );
    if index >= 0 then
      FNeighboursList_From.Delete( index );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge( AEdge: TBasicGraphEdge );
var
  index: Integer;
begin
  if AEdge = nil then
    raise EGraphError.Create( '���������� ������� ����, ��������� ��� ������� (nil).' );

  if ( FEdgesList_To = nil ) OR ( FEdgesList_From = nil ) then
    raise EGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
  try
    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, �������� � ����
    index := FEdgesList_To.IndexOf( AEdge );
    if index >= 0 then
      DeleteEdgeAndNeighbour_To( index, AEdge.NodeFrom );

    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, ��������� �� ����
    index := FEdgesList_From.IndexOf( AEdge );
    if index >= 0 then
      DeleteEdgeAndNeighbour_From( index, AEdge.NodeTo );
  except
    on E:Exception do
      raise EGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� �� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge_ByUID
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByUID( AUID: Int64 );
var
  index: integer;
begin       
  if ( FEdgesList_To = nil ) OR ( FEdgesList_From = nil ) then
    raise EGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
    
  try
    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, �������� � ����
    index := IndexOfEdge_ByUID( gdTo, AUID );
    if index >= 0 then
      DeleteEdgeAndNeighbour_To( index, TBasicGraphEdge( FEdgesList_To[ index ] ).NodeFrom );

    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, ��������� �� ����
    index := IndexOfEdge_ByUID( gdFrom, AUID );
    if index >= 0 then
      DeleteEdgeAndNeighbour_From( index, TBasicGraphEdge( FEdgesList_From[ index ] ).NodeTo );
  except
    on E:Exception do
      raise EGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.DeleteEdge_ByObject
***********************************************************************************************}
procedure TBasicGraphNode.DeleteEdge_ByObject( AObject: TObject );
var
  index: integer;
begin       
  if ( FEdgesList_To = nil ) OR ( FEdgesList_From = nil ) then
    raise EGraphError.Create( '�� ��������������� ������ ���, � ������� ������ ����.' );
    
  try
    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, �������� � ����
    index := IndexOfEdge_ByObject( gdTo, AObject );
    if index >= 0 then
      DeleteEdgeAndNeighbour_To( index, TBasicGraphEdge( FEdgesList_To[ index ] ).NodeFrom );

    //����� � ������� ���� (� ��� �������������, ������� � ����-������) � ������ ���, ��������� �� ����
    index := IndexOfEdge_ByObject( gdFrom, AObject );
    if index >= 0 then
      DeleteEdgeAndNeighbour_From( index, TBasicGraphEdge( FEdgesList_From[ index ] ).NodeTo );
  except
    on E:Exception do
      raise EGraphError.Create( '�� ������� ������� ���� �� ������ ���.' + sLineBreak + '��������� ������:' +
                                      sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.HasNeighbour
* ��������� ������� ����-������ � ������� ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.HasNeighbour( ANode: TBasicGraphNode; AListDirections: TGraphDirectionSet ): Boolean;
begin
  Result := false;

  if ( FNeighboursList_To = nil )
  OR ( FNeighboursList_From = nil) then
   raise EGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( gdTo in AListDirections )
  AND ( FNeighboursList_To.IndexOf( ANode ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( gdFrom in AListDirections )
  AND ( FNeighboursList_From.IndexOf( ANode ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfNeighbour_ByUID
***********************************************************************************************}
function TBasicGraphNode.IndexOfNeighbour_ByUID( AListDirection: TGraphDirection; AUID: Int64 ): Integer;
var
  aNeighboursList: TList;
begin
  Result := -1;
  aNeighboursList := nil;

  if AListDirection = gdTo then
    aNeighboursList := FNeighboursList_To
  else if AListDirection = gdFrom then
    aNeighboursList := FNeighboursList_From;

  //���� ��� ��������� ��� ������ ����, ������� -1
  if ( aNeighboursList.Count = 0 )
  or ( aNeighboursList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aNeighboursList.Count ) and ( TBasicGraphNode( aNeighboursList[ Result ] ).UID <> AUID ) do
    Inc( Result );

  //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
  if aNeighboursList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfNeighbour_ByObject
***********************************************************************************************}
function TBasicGraphNode.IndexOfNeighbour_ByObject( AListDirection: TGraphDirection; AObject: TObject ): Integer;
var
  aNeighboursList: TList;
begin
  Result := -1;
  aNeighboursList := nil;

  if AListDirection = gdTo then
    aNeighboursList := FNeighboursList_To
  else if AListDirection = gdFrom then
    aNeighboursList := FNeighboursList_From;

  //���� ��� ��������� ��� ������ ����, ������� -1
  if ( aNeighboursList.Count = 0 )
  or ( aNeighboursList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aNeighboursList.Count ) and ( TBasicGraphNode( aNeighboursList[ Result ] ).Object_ <> AObject ) do
    Inc( Result );

  //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
  if aNeighboursList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.HasNeighbour_ByUID
*��������� ������� ����-������ � ������� ������� From/To/From&To �� UID
***********************************************************************************************}
function TBasicGraphNode.HasNeighbour_ByUID( AUID: Int64; AListDirections: TGraphDirectionSet ): Boolean;
begin
  Result := false;

  if ( FNeighboursList_To = nil )
  OR ( FNeighboursList_From = nil) then
   raise EGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( gdTo in AListDirections )
  AND ( IndexOfNeighbour_ByUID( gdTo, AUID) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( gdFrom in AListDirections )
  AND ( IndexOfNeighbour_ByUID( gdFrom, AUID) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.HasNeighbour_ByObject
* ��������� ������� ����-������ � ������� ������� From/To/From&To �� ���������������� �������
***********************************************************************************************}
function TBasicGraphNode.HasNeighbour_ByObject( AObject: TObject; AListDirections: TGraphDirectionSet ): Boolean;
begin
  Result := false;

  if ( FNeighboursList_To = nil )
  OR ( FNeighboursList_From = nil) then
    raise EGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( gdTo in AListDirections )
  AND ( IndexOfNeighbour_ByObject( gdTo, AObject) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( gdFrom in AListDirections )
  AND ( IndexOfNeighbour_ByObject( gdFrom, AObject) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighboursCount
* ���������� ���������� �����-������� � ������ From/To
***********************************************************************************************}
function TBasicGraphNode.GetNeighboursCount( AListDirection: TGraphDirection ): Integer;
begin
  Result := -1;
  
  if AListDirection = gdTo then
    Result := FNeighboursList_To.Count
  else if AListDirection = gdFrom then
    Result := FNeighboursList_From.Count
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighbour_ByIndex
* ���������� ���� �� � ������� � ������ From/To
***********************************************************************************************}
function TBasicGraphNode.GetNeighbour_ByIndex( AIndex: Integer; AListDirection: TGraphDirection ): TBasicGraphNode;
begin
  Result := nil;
  
  if ( AIndex < 0 )
  OR ( ( AListDirection = gdTo )
      AND ( AIndex >= FNeighboursList_To.Count ) )
  OR ( ( AListDirection = gdFrom )
      AND ( AIndex >= FNeighboursList_From.Count ) ) then
    Exit;

  if AListDirection = gdTo then
    Result := TBasicGraphNode( FNeighboursList_To[ AIndex ] );

  if AListDirection = gdFrom then
    Result := TBasicGraphNode( FNeighboursList_From[ AIndex ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighbour_ByUID
* ���������� ���� �� � UID, ���� � ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetNeighbour_ByUID( AUID: Int64; AListDirection: TGraphDirection ): TbasicGraphNode;
var
  index: Integer;
begin
  Result := nil;

  index := IndexOfNeighbour_ByUID( AListDirection, AUID );

  if index < 0 then
    Exit;

  if AListDirection = gdTo then
    Result := TBasicGraphNode( FNeighboursList_To[ index ] );

  if AListDirection = gdFrom then
    Result := TBasicGraphNode( FNeighboursList_From[ index ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetNeighbour_ByObject   
* ���������� ���� �� ���������������� � ��� �������, ���� � ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetNeighbour_ByObject( AObject: TObject; AListDirection: TGraphDirection ): TbasicGraphNode;
var
  index: Integer;
begin
  Result := nil;

  index := IndexOfNeighbour_ByObject( AListDirection, AObject );

  if index < 0 then
    Exit;

  if AListDirection = gdTo then
    Result := TBasicGraphNode( FNeighboursList_To[ index ] );

  if AListDirection = gdFrom then
    Result := TBasicGraphNode( FNeighboursList_From[ index ] );
end;

{**********************************************************************************************
* TBasicGraphNode.HasEdge
* ��������� ������� ���� � ������� ��� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.HasEdge( AEdge: TBasicGraphEdge; AListDirections: TGraphDirectionSet ): Boolean;
begin
  Result := false;

  if ( FEdgesList_To = nil )
  OR ( FEdgesList_From = nil) then
   raise EGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( gdTo in AListDirections )
  AND ( FEdgesList_To.IndexOf( AEdge ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( gdFrom in AListDirections )
  AND ( FEdgesList_From.IndexOf( AEdge ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfEdge_ByUID
***********************************************************************************************}
function TBasicGraphNode.IndexOfEdge_ByUID( AListDirection: TGraphDirection; AUID: Int64 ): integer;
var
  aEdgesList: TList;
begin
  Result := -1;
  aEdgesList := nil;

  if AListDirection = gdTo then
    aEdgesList := FEdgesList_To
  else if AListDirection = gdFrom then
    aEdgesList := FEdgesList_From;

  //���� ��� ��������� ��� ������ ����, ������� -1
  if ( aEdgesList.Count = 0 )
  or ( aEdgesList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aEdgesList.Count ) and ( TBasicGraphEdge( aEdgesList[ Result ] ).UID <> AUID ) do
    Inc( Result );

  //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
  if aEdgesList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.IndexOfEdge_ByObject
***********************************************************************************************}
function TBasicGraphNode.IndexOfEdge_ByObject( AListDirection: TGraphDirection; AObject: TObject ): integer;
var
  aEdgesList: TList;
begin
  Result := -1;
  aEdgesList := nil;

  if AListDirection = gdTo then
    aEdgesList := FEdgesList_To
  else if AListDirection = gdFrom then
    aEdgesList := FEdgesList_From;

  //���� ��� ��������� ��� ������ ����, ������� -1
  if ( aEdgesList.Count = 0 )
  or ( aEdgesList = nil ) then
    Exit;

  Result := 0;
  while ( Result < aEdgesList.Count ) and ( TBasicGraphEdge( aEdgesList[ Result ] ).Object_ <> AObject ) do
    Inc( Result );

  //���� ����� �� ����� � �� ����� ����� (result = count), ���� ������� -1
  if aEdgesList.Count = Result then
    Result := -1;
end;

{**********************************************************************************************
* TBasicGraphNode.HasEdge_ByUID
* ��������� ������� ���� � ������� ��� From/To/From&To �� UID
***********************************************************************************************}
function TBasicGraphNode.HasEdge_ByUID( AUID: Int64; AListDirections: TGraphDirectionSet ): Boolean;
begin
  Result := false;

  if ( FEdgesList_To = nil )
  OR ( FEdgesList_From = nil) then
   raise EGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( gdTo in AListDirections )
  AND ( IndexOfEdge_ByUID( gdTo, AUID ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( gdFrom in AListDirections )
  AND ( IndexOfEdge_ByUID( gdFrom, AUID ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.HasEdge_ByObject
* ��������� ������� ���� � ������� ��� From/To/From&To �� ���������������� �������
***********************************************************************************************}
function TBasicGraphNode.HasEdge_ByObject( AObject: TObject; AListDirections: TGraphDirectionSet ): Boolean;
begin
  Result := false;

  if ( FEdgesList_To = nil )
  OR ( FEdgesList_From = nil) then
   raise EGraphError.Create( '�� ���������������� ������ ������� ���� (nil).' );

  //���� � ������ ���� ����������� "To", �� �������� ������� ���� � ������ NeighborsList_To
  if ( gdTo in AListDirections )
  AND ( IndexOfEdge_ByObject( gdTo, AObject ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;

  //���� � ������ ���� ����������� "From", �� �������� ������� ���� � ������ NeighborsList_From
  if ( gdFrom in AListDirections )
  AND ( IndexOfEdge_ByObject( gdFrom, AObject ) >= 0 )
  then
  begin
    Result := True;
    Exit;
  end;
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdgesCount
* ���������� ���������� ��� � ������ From/To
***********************************************************************************************}
function TBasicGraphNode.GetEdgesCount( AListDirection: TGraphDirection ): Integer;
begin
  Result := -1;
  
  if AListDirection = gdTo then
    Result := FEdgesList_To.Count
  else if AListDirection = gdFrom then
    Result := FEdgesList_From.Count
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdge_ByIndex
* ���������� ���� �� � ������� � ������ From/To
***********************************************************************************************}
function TBasicGraphNode.GetEdge_ByIndex( AIndex: Integer; AListDirection: TGraphDirection ): TBasicGraphEdge;
begin
  Result := nil;
  
  if ( AIndex < 0 )
  OR ( ( AListDirection = gdTo )
      AND ( AIndex >= FEdgesList_To.Count ) )
  OR ( ( AListDirection = gdFrom )
      AND ( AIndex >= FEdgesList_From.Count ) ) then
    Exit;

  if AListDirection = gdTo then
    Result := TBasicGraphEdge( FEdgesList_To[ AIndex ] );

  if AListDirection = gdFrom then
    Result := TBasicGraphEdge( FEdgesList_From[ AIndex ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdge_ByUID
* ���������� ���� �� � UID, ���� � ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetEdge_ByUID( AUID: Int64; AListDirection: TGraphDirection ): TbasicGraphEdge;
var
  index: integer;
begin
  Result := nil;

  index := IndexOfEdge_ByUID( AListDirection, AUID );

  if index < 0 then
    Exit;

  if AListDirection = gdTo then
    Result := TBasicGraphEdge( FEdgesList_To[ index ] );

  if AListDirection = gdFrom then
    Result := TBasicGraphEdge( FEdgesList_From[ index ] );
end;

{**********************************************************************************************
* TBasicGraphNode.GetEdge_ByObject
* ���������� ���� �� ���������������� � ��� �������, ���� � ������� From/To/From&To
***********************************************************************************************}
function TBasicGraphNode.GetEdge_ByObject( AObject: TObject; AListDirection: TGraphDirection ): TbasicGraphEdge;
var
  index: integer;
begin
  Result := nil;

  index := IndexOfEdge_ByObject( AListDirection, AObject );

  if index < 0 then
    Exit;

  if AListDirection = gdTo then
    Result := TBasicGraphEdge( FEdgesList_To[ index ] );

  if AListDirection = gdFrom then
    Result := TBasicGraphEdge( FEdgesList_From[ index ] );
end;

//----------------------------------------------------------------------------------------------------------------------
//������ �������� ����
{**********************************************************************************************
* TBasicGraphEdge.SetObject
***********************************************************************************************}
procedure TBasicGraphEdge.SetObject( AObject: TObject );
begin
  Self.FObject := AObject;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetFlOwnsObject
***********************************************************************************************}
procedure TBasicGraphEdge.SetFlOwnsObject( AflOwnsObject: boolean );
begin
  Self.FflOwnsObject := flOwnsObject;
end;

{**********************************************************************************************
* TBasicGraphEdge.GetNodeFrom
***********************************************************************************************}
function TBasicGraphEdge.GetNodeFrom(): TBasicGraphNode;
begin
  Result := Self.FNodeFrom;
end;

{**********************************************************************************************
* TBasicGraphEdge.GetNodeTo
***********************************************************************************************}
function TBasicGraphEdge.GetNodeTo(): TBasicGraphNode;
begin
  Result := Self.FNodeTo;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode1
***********************************************************************************************}
procedure TBasicGraphEdge.SetNodeFrom( ANewNodeFrom: TBasicGraphNode );
begin
  Self.FNodeFrom := ANewNodeFrom;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetNode2
***********************************************************************************************}
procedure TBasicGraphEdge.SetNodeTo( ANewNodeTo: TBasicGraphNode );
begin
  Self.FNodeTo := ANewNodeTo;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetDirection
***********************************************************************************************}
procedure TBasicGraphEdge.SetflBiDirected( AflBiDirected: Boolean );
begin
  Self.FflBiDirected := AflBiDirected;
end;

{**********************************************************************************************
* TBasicGraphEdge.SetWeight
***********************************************************************************************}
procedure TBasicGraphEdge.SetWeight( AWeight: double );
begin
  Self.FWeight := AWeight;
end;

{**********************************************************************************************
* TBasicGraphEdge.Create
***********************************************************************************************}
constructor TBasicGraphEdge.Create( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1;
                                    AObject: TObject = nil; AflOwnsObject: Boolean = false;
                                    AflBiDirected: Boolean = false; AWeight: Double = 1.0 );
begin
  inherited Create();

  if ( ANodeFrom = nil ) or ( ANodeTo = nil ) then
    raise EGraphError.Create( '��� ���� ���� ������ ������������ (�� nil).' );

  FIndex := AIndex;
  FUID := AUID;
  FObject := AObject;
  FflOwnsObject := AflOwnsObject;
  FNodeFrom := ANodeFrom;
  FNodeTo := ANodeTo;
  FWeight := AWeight;
  FflBiDirected := AflBiDirected;

  //����� AddEdge ��� ��������� ���� � ��� ���� �������� ����
  FNodeFrom.AddEdge( Self );
  FNodeTo.AddEdge( Self );
end;

{**********************************************************************************************
* TBasicGraphEdge.Destroy
***********************************************************************************************}
destructor TBasicGraphEdge.Destroy();
begin
  FNodeTo := nil;
  FNodeFrom := nil;

  if FflOwnsObject then
    FreeAndNil( FObject );

  inherited Destroy();
end;

//----------------------------------------------------------------------------------------------------------------------
//������ �������� �����
{**********************************************************************************************
* TBasicGraph.Create
***********************************************************************************************}
constructor TBasicGraph.Create();
begin
  inherited Create();

  FHighestNodeIndexValue := 0;
  FHIghestEdgeIndexValue := 0;

  FNodes_ByUID := THashContainer.Create( True );
  FNodes_ByObject := THashContainer.Create( );
  FEdges_ByUID := THashContainer.Create( True );
  FEdges_ByObject := THashContainer.Create( );
end;

destructor TBasicGraph.Destroy();
begin
  FreeAndNil( FNodes_ByUID );
  FreeAndNil( FNodes_ByObject );
  FreeAndNil( FEdges_ByUID );
  FreeAndNil( FEdges_ByObject );

  inherited Destroy();
end;

{**********************************************************************************************
* TBasicGraph.CreateNode
***********************************************************************************************}
function TBasicGraph.CreateNode( AUID: Int64 = -1; AIndex: Integer = -1; AObject: TObject = nil;
                                  AflOwnsObject: Boolean = False ): TBasicGraphNode;
begin
  Result := TBasicGraphNode.Create( AUID, AIndex, AObject, AflOwnsObject );
end;

{**********************************************************************************************
* TBasicGraph.AddNode
* �������� ���� � ������ �����, ���������� ������
***********************************************************************************************}
function TBasicGraph.AddNode( AUID: Int64 = -1; AObject: TObject = nil; AflOwnsObject: Boolean = False ): TBasicGraphNode;
var
  node: TBasicGraphNode;
begin
  if ( FNodes_ByUID = nil )
  OR ( FNodes_ByObject = nil) then
    raise EGraphError.Create( '���������� �������� ���� � �������������� (nil) ������ �����.' );

  try
    node := CreateNode( AUID, FHighestNodeIndexValue, AObject, AflOwnsObject );
    FHighestNodeIndexValue := FHighestNodeIndexValue + 1;

    FNodes_ByUID.AddObject( node.UID, node, true );
    if AObject <> nil then
      FNodes_ByObject.AddObject( Integer( node.Object_ ), node, true );
  except
    on E:Exception do
      raise EGraphError.Create( '�� ������� �������� ����. ��������� ������:' + sLineBreak + E.Message );
  end;

  Result := node;
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode
***********************************************************************************************}
function TBasicGraph.DeleteNode( ANode: TBasicGraphNode;
                                 ADeletionMode: TGraphDeletionMode = gdmAllowOnlyTarget ): Boolean;
var
  index: integer;
begin
  //��� ��������� ����, �� ������� ���� ������� � ����, � ������� �� ������
  Result := false;

  if ( ANode.GetEdgesCount( gdTo ) + ANode.GetEdgesCount( gdFrom ) > 0 )
  and ( ADeletionMode = gdmAllowOnlyTarget ) then
    Exit;

  try
    //������� �� ����� ����, �������� � ����
    for index := 0 to ANode.GetEdgesCount( gdTo ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, gdTo ) );

    //������� �� ����� ����, ��������� �� ����
    for index := 0 to ANode.GetEdgesCount( gdFrom ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, gdFrom ) );

    //������� �� ����� ����
    FNodes_ByUID.DelObject( ANode.UID, ANode );
    FNodes_ByObject.DelObject( Integer( ANode.Object_ ), ANode );

    Result := true;
  except
    on E: Exception do
      raise EGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByUID
***********************************************************************************************}
function TBasicGraph.DeleteNode_ByUID( AUID: Int64;
                                       ADeletionMode: TGraphDeletionMode = gdmAllowOnlyTarget ): Boolean;
var
  index: integer;
  aNode: TBasicGraphNode;
begin
  //��� ��������� ����, �� ������� ���� ������� � ����, � ������� �� ������
  Result := false;

  aNode := GetNode_ByUID( AUID );

  if ( aNode.GetEdgesCount( gdTo ) + aNode.GetEdgesCount( gdFrom ) > 0 )
  and ( ADeletionMode = gdmAllowOnlyTarget ) then
    Exit;

  try
    //������� �� ����� ����, �������� � ����
    for index := 0 to ANode.GetEdgesCount( gdTo ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, gdTo ) );

    //������� �� ����� ����, ��������� �� ����
    for index := 0 to ANode.GetEdgesCount( gdFrom ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, gdFrom ) );

    //������� �� ����� ����
    FNodes_ByUID.DelObject( ANode.UID, ANode );
    FNodes_ByObject.DelObject( Integer( ANode.Object_ ), ANode );

    Result := true;
  except
    on E: Exception do
      raise EGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteNode_ByObject
***********************************************************************************************}
function TBasicGraph.DeleteNode_ByObject( AObject: TObject;
                                       ADeletionMode: TGraphDeletionMode = gdmAllowOnlyTarget ): Boolean;
var
  index: integer;
  aNode: TBasicGraphNode;
begin
  //��� ��������� ����, �� ������� ���� ������� � ����, � ������� �� ������
  Result := false;

  aNode := GetNode_ByObject( AObject );

  if ( aNode.GetEdgesCount( gdTo ) + aNode.GetEdgesCount( gdFrom ) > 0 )
  and ( ADeletionMode = gdmAllowOnlyTarget ) then
    Exit;

  try
    //������� �� ����� ����, �������� � ����
    for index := 0 to ANode.GetEdgesCount( gdTo ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, gdTo ) );

    //������� �� ����� ����, ��������� �� ����
    for index := 0 to ANode.GetEdgesCount( gdFrom ) - 1 do
      DeleteEdge( ANode.GetEdge_ByIndex( index, gdFrom ) );

    //������� �� ����� ����
    FNodes_ByUID.DelObject( ANode.UID, ANode );
    FNodes_ByObject.DelObject( Integer( ANode.Object_ ), ANode );

    Result := true;
  except
    on E: Exception do
      raise EGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.GetNode_ByUID
* �������� ���� �� ��� UID
***********************************************************************************************}
function TBasicGraph.GetNode_ByUID( AUID: Int64 ): TBasicGraphNode;
begin
  Result := TBasicGraphNode( FNodes_ByUID.GetObject( AUID ) );
end;

{**********************************************************************************************
* TBasicGraph.GetNode_ByObject
***********************************************************************************************}
function TBasicGraph.GetNode_ByObject( AObject: TObject ): TBasicGraphNode;
begin
  Result := TbasicGraphNode( FNodes_ByObject.GetObject( Integer( AObject ) ) );
end;

{**********************************************************************************************
* TBasicGraph.CreateEdge
***********************************************************************************************}
function TBasicGraph.CreateEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1; AIndex: Integer = -1;
                                  AObject: TObject = nil; AflOwnsObject: Boolean = false;
                                  AflBiDirected: Boolean = false; AWeight: Double = 1.0 ): TBasicGraphEdge;
begin
  Result := TBasicGraphEdge.Create( ANodeFrom, ANodeTo, AUID, AIndex, AObject, AflOwnsObject,
                                    AflBiDirected, AWeight );
end;

{**********************************************************************************************
*  TBasicGraph.AddEdge
***********************************************************************************************}
function TBasicGraph.AddEdge( ANodeFrom, ANodeTo: TBasicGraphNode; AUID: Int64 = -1;
                              AObject: TObject = nil; AflOwnsObject: Boolean = false;
                              AflBiDirected: Boolean = false; AWeight: Double = 1.0 ): TBasicGraphEdge;
var
  edge: TBasicGraphEdge;
begin
  if ( FEdges_ByUID = nil )
  OR ( FEdges_ByObject = nil) then
    raise EGraphError.Create( '���������� �������� ���� � �������������� (nil) ������ ���.' );

  try
    edge := CreateEdge( ANodeFrom, ANodeTo, AUID, FHighestEdgeIndexValue, AObject, AflOwnsObject, AflBiDirected, AWeight );
    FHighestEdgeIndexValue := FHighestEdgeIndexValue + 1;

    FEdges_ByUID.AddObject( edge.UID, edge, true );
    if AObject <> nil then
      FEdges_ByObject.AddObject( Integer( edge.Object_ ), edge, true );
  except
    on E:Exception do
      raise EGraphError.Create( '�� ������� �������� ����. ��������� ������:' + sLineBreak + E.Message );
  end;

  Result := edge;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge
* ������� ���� �� ������ ���
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge( AEdge: TBasicGraphEdge );
begin
  try
    //�������� ���� �� ������� ���, ��������� � ������ To � From ���� ����
    AEdge.NodeTo.DeleteEdge( AEdge );
    AEdge.NodeFrom.DeleteEdge( AEdge );

    //�������� ���� �� ������� ��� �����
    FEdges_ByUID.DelObject( AEdge.UID, AEdge );
    FEdges_ByObject.DelObject( Integer( AEdge.Object_ ), AEdge );
  except
    on E: Exception do
      raise EGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge_ByUID
* ������� ���� �� ������, ����� �� Unique ID
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByUID( AUID: Int64 );
var
  aEdge: TBasicGraphEdge;
begin
  aEdge := GetEdge_ByUID( AUID );

  try
    //�������� ���� �� ������� ���, ��������� � ������ To � From ���� ����
    aEdge.NodeTo.DeleteEdge( aEdge );
    aEdge.NodeFrom.DeleteEdge( aEdge );

    //�������� ���� �� ������� ��� �����
    FEdges_ByUID.DelObject( aEdge.UID, aEdge );
    FEdges_ByObject.DelObject( Integer( aEdge.Object_ ), aEdge );
  except
    on E: Exception do
      raise EGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.DeleteEdge_ByObject
* ������� ���� �� �������
***********************************************************************************************}
procedure TBasicGraph.DeleteEdge_ByObject( AObject: TObject );
var
  aEdge: TBasicGraphEdge;
begin
  aEdge := GetEdge_ByObject( AObject );

  try
    //�������� ���� �� ������� ���, ��������� � ������ To � From ���� ����
    aEdge.NodeTo.DeleteEdge( aEdge );
    aEdge.NodeFrom.DeleteEdge( aEdge );

    //�������� ���� �� ������� ��� �����
    FEdges_ByUID.DelObject( aEdge.UID, aEdge );
    FEdges_ByObject.DelObject( Integer( aEdge.Object_ ), aEdge );
  except
    on E: Exception do
      raise EGraphError.Create( '�� ������� ������� ����. ��������� ������: ' + sLineBreak + E.Message );
  end;
end;

{**********************************************************************************************
* TBasicGraph.GetEdge_ByUID
* �������� ���� �� � UID
***********************************************************************************************}
function TBasicGraph.GetEdge_ByUID( AUID: Int64 ): TBasicGraphEdge;
begin
  Result := TBasicGraphEdge( FEdges_ByUID.GetObject( AUID ) );
end;

{**********************************************************************************************
* TBasicGraph.GetEdge_ByObject
* ��������� ���� �� � �������
***********************************************************************************************}
function TBasicGraph.GetEdge_ByObject( AObject: TOBject ): TBasicGraphEdge;
begin
  Result := TBasicGraphEdge( FEdges_ByObject.GetObject( Integer( AObject ) ) );
end;

{**********************************************************************************************
* TBasicGraph.AddNode_ToEdge
***********************************************************************************************}
function TBasicGraph.AddNode_ToEdge( AEdge: TBasicGraphEdge; ANode: TBasicGraphNode; AEdge_NewWeight: Double = 1.0;
                                      ANewEdge_UID: Int64 = -1; ANewEdge_Object: TObject = nil;
                                      ANewEdge_flOwnsObject: Boolean = false ):
                                      Boolean;
var
  newEdge: TBasicGraphEdge;
  newEdge_Weight: Double;
begin
  Result := False;

  if ( FNodes_ByUID = nil )
  OR ( FNodes_ByObject = nil) then
    raise EGraphError.Create( '���������� �������� ���� � �������������� (nil) ������ �����.' );

  newEdge_Weight := AEdge.Weight - AEdge_NewWeight;
  if ( newEdge_Weight >= AEdge.Weight )
  OR ( newEdge_Weight <= 0.0 ) then
    Exit;

  try
    AEdge.NodeTo.DeleteEdge( AEdge );
    newEdge := AddEdge( ANode, AEdge.NodeTo, ANewEdge_UID, ANewEdge_Object, ANewEdge_flOwnsObject,
                        AEdge.FlBiDirected, newEdge_Weight );
    AEdge.NodeTo := ANode;
    ANode.AddEdge( AEdge );    
    AEdge.Weight := AEdge_NewWeight;
  except
    on E:Exception do
      raise EGraphError.Create( '�� ������� �������� ����. ��������� ������:' + sLineBreak + E.Message );
  end;

  Result := True;

end;

{**********************************************************************************************
* ������� ���� � ���������� �������� � ��������� ���� � ����.
***********************************************************************************************}
function TBasicGraph.DeleteNode_AndMergeEdges( ANode: TBasicGraphNode ): Boolean;
var
  edgeTo, edgeFrom: TBasicGraphEdge;
begin
  Result := false;

  //���� � ���� �� 1 �������� ��� �� 1 ��������� ��� �������� = ��������� ����, �� ����� � ������� false
  if ( ANode.GetEdgesCount( gdTo ) <> 1 )
    or ( ANode.GetEdgesCount( gdFrom ) <> 1 )
    or ( ANode.GetEdge_ByIndex( 0, gdTo ) = ANode.GetEdge_ByIndex(0, gdFrom ) )
  then
    Exit;

  //�������� ����� � ������ ����
  edgeTo := ANode.GetEdge_ByIndex(0, gdTo);
  edgeFrom := ANode.getEdge_ByIndex(0, gdFrom);
  //�������� ��������� ����� ����, � ��� ����� ������ � nodeTo
  edgeTo.Weight := edgeTo.Weight + edgeFrom.Weight;
  edgeTo.NodeTo := edgeFrom.NodeTo;
  //������ ������ � ���� �� ���������� ����, ����� �� � �� ������
  ANode.DeleteEdge( edgeTo );
  //������ ���� - �� ������ ��������� ���� ��������������
  DeleteNode( ANode );
  //��������� ����� ���� � ������ � ������ ����
  edgeTo.NodeTo.AddEdge( edgeTo );

  Result := true;
end;

{**********************************************************************************************
* ������� ���� �� UID � ���������� �������� � ��������� ���� � ����.
***********************************************************************************************}
function TBasicGraph.DeleteNode_AndMergeEdges_ByUID( AUID: Int64 ): Boolean;
var
  edgeTo, edgeFrom: TBasicGraphEdge;
  aNode: TBasicGraphNode;
begin
  Result := false;

  aNode := GetNode_ByUID( AUID );

  //���� � ���� �� 1 �������� ��� �� 1 ��������� ��� �������� = ��������� ����, �� ����� � ������� false
  if ( aNode.GetEdgesCount( gdTo ) <> 1 )
    or ( aNode.GetEdgesCount( gdFrom ) <> 1 )
    or ( aNode.GetEdge_ByIndex( 0, gdTo ) = aNode.GetEdge_ByIndex(0, gdFrom ) )
  then
    Exit;

  //�������� ����� � ������ ����
  edgeTo := aNode.GetEdge_ByIndex(0, gdTo);
  edgeFrom := aNode.getEdge_ByIndex(0, gdFrom);
  //�������� ��������� ����� ����, � ��� ����� ������ � nodeTo
  edgeTo.Weight := edgeTo.Weight + edgeFrom.Weight;
  edgeTo.NodeTo := edgeFrom.NodeTo;
  //������ ������ � ���� �� ���������� ����, ����� �� � �� ������
  aNode.DeleteEdge( edgeTo );
  //������ ���� - �� ������ ��������� ���� ��������������
  DeleteNode( aNode );
  //��������� ����� ���� � ������ � ������ ����
  edgeTo.NodeTo.AddEdge( edgeTo );

  Result := true;
end;

{**********************************************************************************************
* ������� ���� �� ������� � ���������� �������� � ��������� ���� � ����.
***********************************************************************************************}
function TBasicGraph.DeleteNode_AndMergeEdges_ByObject( AObject: TObject ): Boolean;
var
  edgeTo, edgeFrom: TBasicGraphEdge;
  aNode: TBasicGraphNode;
begin
  Result := false;

  aNode := GetNode_ByObject( AObject );

  //���� � ���� �� 1 �������� ��� �� 1 ��������� ��� �������� = ��������� ����, �� ����� � ������� false
  if ( aNode.GetEdgesCount( gdTo ) <> 1 )
    or ( aNode.GetEdgesCount( gdFrom ) <> 1 )
    or ( aNode.GetEdge_ByIndex( 0, gdTo ) = aNode.GetEdge_ByIndex(0, gdFrom ) )
  then
    Exit;

  //�������� ����� � ������ ����
  edgeTo := aNode.GetEdge_ByIndex(0, gdTo);
  edgeFrom := aNode.getEdge_ByIndex(0, gdFrom);
  //�������� ��������� ����� ����, � ��� ����� ������ � nodeTo
  edgeTo.Weight := edgeTo.Weight + edgeFrom.Weight;
  edgeTo.NodeTo := edgeFrom.NodeTo;
  //������ ������ � ���� �� ���������� ����, ����� �� � �� ������
  aNode.DeleteEdge( edgeTo );
  //������ ���� - �� ������ ��������� ���� ��������������
  DeleteNode( aNode );
  //��������� ����� ���� � ������ � ������ ����
  edgeTo.NodeTo.AddEdge( edgeTo );

  Result := true;
end;

{**********************************************************************************************
*TBasicGraph.GetAppropriateEdges_for_FindPath
***********************************************************************************************}
function TBasicGraph.GetAppropriateEdges_for_FindPath( ANode: TBasicGraphNode ): TList;
var
  i: integer;
begin
  Result := TList.Create();
  for i := 0 to ANode.GetEdgesCount( gdFrom ) - 1 do
    Result.Add( ANode.GetEdge_ByIndex( i, gdFrom ) );
end;

{**********************************************************************************************
* TBasicGraph.FindPath
***********************************************************************************************}
function TBasicGraph.FindPath( nodeFrom, nodeTo: TBasicGraphNode; path: TList ): double;
var
  distanceFromSource: array of Double;
  incomingEdge: array of TBasicGraphEdge;
  queuedEdges: TEdgesQueueWithPriority;
  index: integer;
  currentNode: TBasicGraphNode;
  queuedEdge, currentEdge: TBasicGraphEdge;
  edgeStruct: TEdgeStruct;
  currentDistanceFromSource: Double;
  appropriateEdges: TList;

  procedure AddStartEdgesToArray( startNode: TBasicGraphNode );
  var
    index: Integer;
    edge: TBasicGraphEdge;
  begin
    for index := 0 to startNode.GetEdgesCount( gdFrom ) - 1 do
    begin
      edge := startNode.GetEdge_ByIndex( index, gdFrom );
      distanceFromSource[ edge.NodeTo.NodeIndex ] := edge.Weight;
      incomingEdge[ edge.NodeTo.NodeIndex ] := edge;
      queuedEdges.AddElement( TEdgeStruct.Create( edge, edge.Weight ) );
    end;
  end;

begin
  if nodeFrom = nodeTo then
  begin
    Result := 0;
    exit;
  end;

  Result := -1;
  try
    SetLength( distanceFromSource, FNodes_ByUID.ObjectCount );
    SetLength( incomingEdge, FNodes_ByUID.ObjectCount );
    queuedEdges := TEdgesQueueWithPriority.Create();
    //-------------------------------------------------------------------------------------------------------------------
    //�������������
    for index := 0 to FNodes_ByUID.ObjectCount - 1 do
    begin
      distanceFromSource[ index ] := -1.0;
      incomingEdge[ index ] := nil;
    end;

    //���������� �� Source �� Source = 0
    //������ �������������, �� ������ ����� ������ �������, ��������� � Source - ������ ��� �� ���
    distanceFromSource[ nodeFrom.NodeIndex ] := 0;
    incomingEdge[ nodeFrom.NodeIndex ] := nil;
    AddStartEdgesToArray( nodeFrom );

    //-------------------------------------------------------------------------------------------------------------------
    //������ ��� ���������
    while ( not queuedEdges.IsEmpty() ) do
    begin
      edgeStruct := queuedEdges.GetElement();
      if ( edgeStruct.Cost <> distanceFromSource[ edgeStruct.Edge.NodeTo.NodeIndex ] ) then
      begin
        edgeStruct.Edge := nil;
        FreeAndNil( edgeStruct );
        continue;
      end;

      queuedEdge := edgeStruct.Edge;
      currentNode := queuedEdge.NodeTo;
      appropriateEdges := GetAppropriateEdges_for_FindPath( currentNode );
      for index := 0 to appropriateEdges.Count - 1 do
      begin
        currentEdge := TBasicGraphEdge( appropriateEdges[ index ] );
        currentDistanceFromSource := distanceFromSource[ currentNode.NodeIndex ] + currentEdge.Weight;
        if ( distanceFromSource[ currentEdge.NodeTo.NodeIndex ] < 0 )
          OR ( distanceFromSource[ currentEdge.NodeTo.NodeIndex ] > currentDistanceFromSource ) then
        begin
          distanceFromSource[ currentEdge.NodeTo.NodeIndex ] := currentDistanceFromSource;
          incomingEdge[ currentEdge.NodeTo.NodeIndex ] := currentEdge;
          queuedEdges.AddElement( TEdgeStruct.Create( currentEdge, currentDistanceFromSource ) );
        end;
      end;    
      FreeAndNil( edgeStruct );
      FreeAndNil( appropriateEdges );
    end;
    FreeAndNil( queuedEdges );

    //-------------------------------------------------------------------------------------------------------------------
    //�������� ��� ���������
    currentEdge := incomingEdge[ nodeTo.NodeIndex ];
    if currentEdge <> nil then
      Result := 0;
      
    while currentEdge <> nil do
    begin
      path.Insert( 0, currentEdge );
      Result := Result + currentEdge.Weight;
      currentEdge := incomingEdge[ currentEdge.NodeFrom.NodeIndex ];
    end;
  finally
    FreeAndNil( queuedEdges );
  end;
end;

end.
