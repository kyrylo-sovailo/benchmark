program GraphBenchmark;

uses
    SysUtils, StrUtils, Math, Generics.Collections;

type

    TBenchmark = record
        source: Cardinal;
        destination: Cardinal;
    end;

    TConnection = record
        destination: Cardinal;
        distance: Single;
    end;

    TConnections = TList<TConnection>;

    TCandidate = record
        id: Cardinal;
        int_distance: Cardinal;
        distance: Single;
    end;

    TIndexedQueue = class
    private
        fdata: TList<TCandidate>;
        findices: array of Cardinal;
    public
        constructor Create(count: Integer);
        destructor Destroy; override;
        procedure Push(item: TCandidate);
        function Pop : TCandidate;
        function Count : Integer;
        procedure Clear;
    end;

constructor TIndexedQueue.Create(count: Integer);
begin
    fdata := TList<TCandidate>.Create;
    fdata.Capacity := count;
    SetLength(findices, count);
end;

procedure TIndexedQueue.Push(item: TCandidate);
var
    index, parent_index: Cardinal;
    parent_exists, index_moved: Boolean;
begin
    index := findices[item.id];
    if index = Cardinal(-1) then
    begin
        index := fdata.Count; //value used only when Count == 0, otherwise only for allocation
        fdata.Add(item);
    end
    else if index = Cardinal(-2) then
        Exit
    else
    begin
        if item.distance >= fdata[index].distance then
            Exit;
    end;

    while True do
    begin
        parent_exists := index > 0;
        index_moved := False;
        if parent_exists then
        begin
            parent_index := (index - 1) div 2;
            if item.distance < fdata[parent_index].distance then
            begin
                fdata[index] := fdata[parent_index];
                findices[fdata[index].id] := index;
                index := parent_index;
                index_moved := True;
            end
        end;
        if not index_moved then
        begin
            fdata[index] := item;
            findices[item.id] := index;
            break;
        end
    end;
end;

destructor TIndexedQueue.Destroy;
begin
    fdata.Free;
    inherited;
end;

function TIndexedQueue.Pop : TCandidate;
var
    index, left_index, right_index, next_index: Cardinal;
    left_exists, right_exists, index_moved: Boolean;
    back: TCandidate;
begin
    Pop := fdata[0];
    findices[fdata[0].id] := Cardinal(-2);
    if fdata.Count = 1 then
    begin
        fdata.Delete(0);
        Exit;
    end;

    back := fdata[fdata.Count - 1];
    index := 0;
    while True do
    begin
        left_index := 2 * index + 1;
        right_index := 2 * index + 2;
        left_exists := left_index < fdata.Count;
        right_exists := right_index < fdata.Count;

        index_moved := False;
        if left_exists or right_exists then
        begin
            if left_exists and right_exists then
            begin
                if fdata[left_index].distance < fdata[right_index].distance then
                    next_index := left_index
                else
                    next_index := right_index;
            end
            else
                next_index := left_index;

            if fdata[next_index].distance < back.distance then
            begin
                fdata[index] := fdata[next_index];
                findices[fdata[index].id] := index;
                index := next_index;
                index_moved := True;
            end;
        end;

        if not index_moved then
        begin
            fdata[index] := back;
            findices[back.id] := index;
            fdata.Delete(fdata.Count - 1);
            Break;
        end;
    end;
end;

function TIndexedQueue.Count : Integer;
begin
    Count := fdata.Count;
end;

procedure TIndexedQueue.Clear;
var
    i: Cardinal;
begin
    fdata.Count := 0;
    for i := 0 to Length(findices) - 1 do
        findices[i] := Cardinal(-1);
end;

procedure ParseVer5(graph: TList<TConnections>; benchmarks: TList<TBenchmark>);
var
    f: TextFile;
    line: string;
    split: TStringArray;
    benchmark: TBenchmark;
    connection: TConnection;
    source, destination: Cardinal;
    old_size, grow_i: Cardinal;
    read_benchmarks: Boolean;
begin
    Assign(f, 'dijkstra.txt');
    Reset(f);

    read_benchmarks := False;

    while not Eof(f) do
    begin
        ReadLn(f, line);
        
        if Pos('GRAPH', line) > 0 then 
        begin
            read_benchmarks := False; 
            Continue;
        end
        else if Pos('BENCHMARK', line) > 0 then 
        begin
            read_benchmarks := True; 
            Continue;
        end;

        split := SplitString(line, ' ');
        if read_benchmarks then
        begin
            if TryStrToUInt(split[0], benchmark.source) and TryStrToUInt(split[1], benchmark.destination) then
                benchmarks.Add(benchmark)
            else
                Break;
        end
        else
        begin
            if TryStrToUInt(split[0], source) and TryStrToUInt(split[1], destination) and TryStrToFloat(split[2], connection.distance) then
            begin
                if Max(source, destination) + 1 > graph.Count then
                begin
                    old_size := graph.Count;
                    graph.Count := Max(source, destination) + 1;
                    for grow_i := old_size to graph.Count - 1 do
                        graph[grow_i] := TConnections.Create;
                end;
                connection.destination := destination;
                graph[source].Add(connection);
                connection.destination := source;
                graph[destination].Add(connection);
            end
            else Break;
        end;
    end;

    Close(f);
end;

procedure SolveVer5(graph: TList<TConnections>; benchmarks: TList<TBenchmark>);
var
    //Queue of candidates
    candidates: TIndexedQueue;
    candidate: TCandidate;
    //Temporary values
    new_candidate: TCandidate;
    //Iteration
    benchmark: TBenchmark;
    connection: TConnection;
    //Result
    distance: Single;
    int_distance: Cardinal;
begin
    candidates := TIndexedQueue.Create(graph.Count);
    
    for benchmark in benchmarks do
    begin
        candidates.Clear;
        
        candidate.id := benchmark.source;
        candidate.int_distance := 0;
        candidate.distance := 0.0;
        candidates.Push(candidate);
        
        distance := Infinity;
        int_distance := 0;
        
        while candidates.Count <> 0 do
        begin
            candidate := candidates.Pop;
            if candidate.id = benchmark.destination then 
            begin
                distance := candidate.distance;
                int_distance := candidate.int_distance;
                Break; 
            end;
            
            for connection in graph[candidate.id] do
            begin
                new_candidate.id := connection.destination;
                new_candidate.distance := candidate.distance + connection.distance;
                new_candidate.int_distance := candidate.int_distance + 1;
                candidates.Push(new_candidate);
            end;
        end;

        Write(benchmark.source, ' -> ', benchmark.destination, ': ');
        WriteLn(FloatToStrF(distance, ffFixed, 100, 6), ' (', int_distance, ')');
    end;
end;

procedure MainVer5;
var
    graph: TList<TConnections>;
    benchmarks: TList<TBenchmark>;
begin
    graph := TList<TConnections>.Create;
    benchmarks := TList<TBenchmark>.Create;
    try
        ParseVer5(graph, benchmarks);
        SolveVer5(graph, benchmarks);
    finally
        graph.Free;
        benchmarks.Free;
    end;
end;

begin
    MainVer5();
end.
