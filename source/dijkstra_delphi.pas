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
    i, parent_i: Cardinal;
    b1: Cardinal;
    b2: TCandidate;
begin
    i := findices[item.id];
    if i = Cardinal(-1) then
    begin
        i := fdata.Count;
        findices[item.id] := i;
        fdata.Add(item);
    end
    else if i = Cardinal(-2) then
        Exit
    else
    begin
        if item.distance < fdata[i].distance then
            fdata[i] := item
        else
            Exit;
    end;

    while i > 0 do
    begin
        parent_i := (i - 1) div 2;
        if fdata[i].distance < fdata[parent_i].distance then
        begin
            b1 := findices[fdata[i].id]; findices[fdata[i].id] := findices[fdata[parent_i].id]; findices[fdata[parent_i].id] := b1;
            b2 := fdata[i]; fdata[i] := fdata[parent_i]; fdata[parent_i] := b2;
            i := parent_i;
        end
        else
            Break;
    end;
end;

destructor TIndexedQueue.Destroy;
begin
    fdata.Free;
    inherited;
end;

function TIndexedQueue.Pop : TCandidate;
var
    i, left_i, right_i: Cardinal;
    b1: Cardinal;
    b2: TCandidate;
begin
    Pop := fdata[0];
    findices[fdata[fdata.Count - 1].id] := 0;
    findices[fdata[0].id] := Cardinal(-2);
    fdata[0] := fdata[fdata.Count - 1];
    fdata.Delete(fdata.Count - 1);

    i := 0;
    while True do
    begin
        left_i := 2 * i + 1;
        right_i := 2 * i + 2;
        if (left_i < fdata.Count) and (right_i < fdata.Count) then
        begin
            if fdata[left_i].distance < fdata[right_i].distance then
            begin
                if fdata[left_i].distance < fdata[i].distance then
                begin
                    b1 := findices[fdata[i].id]; findices[fdata[i].id] := findices[fdata[left_i].id]; findices[fdata[left_i].id] := b1;
                    b2 := fdata[i]; fdata[i] := fdata[left_i]; fdata[left_i] := b2;
                    i := left_i;
                end
                else
                    Break;
            end
            else
            begin
                if fdata[right_i].distance < fdata[i].distance then
                begin
                    b1 := findices[fdata[i].id]; findices[fdata[i].id] := findices[fdata[right_i].id]; findices[fdata[right_i].id] := b1;
                    b2 := fdata[i]; fdata[i] := fdata[right_i]; fdata[right_i] := b2;
                    i := right_i;
                end
                else Break;
            end;
        end
        else if left_i < fdata.Count then
        begin
            if fdata[left_i].distance < fdata[i].distance then
            begin
                b1 := findices[fdata[i].id]; findices[fdata[i].id] := findices[fdata[left_i].id]; findices[fdata[left_i].id] := b1;
                b2 := fdata[i]; fdata[i] := fdata[left_i]; fdata[left_i] := b2;
                i := left_i;
            end
            else
                Break;
        end
        else
            Break;
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
