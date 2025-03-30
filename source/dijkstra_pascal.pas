program GraphBenchmark;

uses
    SysUtils, StrUtils, Math;

type
    TBenchmark = record
        source: Cardinal;
        destination: Cardinal;
    end;

    TBenchmarkVector = record
        p: array of TBenchmark;
        size: Cardinal;
    end;

    TConnection = record
        destination: Cardinal;
        distance: Single;
    end;

    TConnectionVector = record
        p: array of TConnection;
        size: Cardinal;
    end;

    TConnectionVectorVector = record
        p: array of TConnectionVector;
        size: Cardinal;
    end;

    TCandidate = record
        id: Cardinal;
        int_distance: Cardinal;
        distance: Single;
    end;

procedure PushBenchmarkVector(var benchmarks: TBenchmarkVector; item: TBenchmark);
var
    new_capacity : Cardinal;
begin
    if benchmarks.size + 1 > Length(benchmarks.p) then
    begin
        if Length(benchmarks.p) = 0 then
            new_capacity := 1
        else
            new_capacity := 2 * Length(benchmarks.p);
        SetLength(benchmarks.p, new_capacity);
    end;
    benchmarks.p[benchmarks.size] := item;
    Inc(benchmarks.size);
end;

procedure PushConnectionVector(var connections: TConnectionVector; item: TConnection);
var
    new_capacity : Cardinal;
begin
    if connections.size + 1 > Length(connections.p) then
    begin
        if Length(connections.p) = 0 then
            new_capacity := 1
        else
            new_capacity := 2 * Length(connections.p);
        SetLength(connections.p, new_capacity);
    end;
    connections.p[connections.size] := item;
    Inc(connections.size);
end;

procedure GrowConnectionVectorVector(var graph: TConnectionVectorVector; size: Cardinal);
var
    new_capacity : Cardinal;
begin
    if size <= graph.size then Exit;
    if size > Length(graph.p) then
    begin
        if Length(graph.p) = 0 then
            new_capacity := 1
        else
            new_capacity := Length(graph.p);
        while size > new_capacity do
            new_capacity := 2 * new_capacity;
        SetLength(graph.p, new_capacity);
    end;
    graph.size := size;
end;

procedure PushIndexHeap(var data: array of TCandidate; var size: Cardinal; var indices: array of Cardinal; item: TCandidate);
var
    i, parent_i: Cardinal;
    b1: Cardinal;
    b2: TCandidate;
begin
    i := indices[item.id];
    if i = Cardinal(-1) then
    begin
        i := size;
        indices[item.id] := i;
        data[i] := item;
        Inc(size);
    end
    else if i = Cardinal(-2) then
        Exit
    else
    begin
        if item.distance < data[i].distance then
            data[i] := item
        else
            Exit;
    end;

    while i > 0 do
    begin
        parent_i := (i - 1) div 2;
        if data[i].distance < data[parent_i].distance then
        begin
            b1 := indices[data[i].id]; indices[data[i].id] := indices[data[parent_i].id]; indices[data[parent_i].id] := b1;
            b2 := data[i]; data[i] := data[parent_i]; data[parent_i] := b2;
            i := parent_i;
        end
        else
            Break;
    end;
end;

function PopIndexHeap(var data: array of TCandidate; var size: Cardinal; var indices: array of Cardinal): TCandidate;
var
    i, left_i, right_i: Cardinal;
    b1: Cardinal;
    b2: TCandidate;
begin
    PopIndexHeap := data[0];
    indices[data[0].id] := Cardinal(-2);
    indices[data[size - 1].id] := 0;
    data[0] := data[size - 1];
    Dec(size);

    i := 0;
    while True do
    begin
        left_i := 2 * i + 1;
        right_i := 2 * i + 2;
        if (left_i < size) and (right_i < size) then
        begin
            if data[left_i].distance < data[right_i].distance then
            begin
                if data[left_i].distance < data[i].distance then
                begin
                    b1 := indices[data[i].id]; indices[data[i].id] := indices[data[left_i].id]; indices[data[left_i].id] := b1;
                    b2 := data[i]; data[i] := data[left_i]; data[left_i] := b2;
                    i := left_i;
                end
                else
                    Break;
            end
            else
            begin
                if data[right_i].distance < data[i].distance then
                begin
                    b1 := indices[data[i].id]; indices[data[i].id] := indices[data[right_i].id]; indices[data[right_i].id] := b1;
                    b2 := data[i]; data[i] := data[right_i]; data[right_i] := b2;
                    i := right_i;
                end
                else Break;
            end;
        end
        else if left_i < size then
        begin
            if data[left_i].distance < data[i].distance then
            begin
                b1 := indices[data[i].id]; indices[data[i].id] := indices[data[left_i].id]; indices[data[left_i].id] := b1;
                b2 := data[i]; data[i] := data[left_i]; data[left_i] := b2;
                i := left_i;
            end
            else
                Break;
        end
        else
            Break;
    end;
end;

procedure ParseVer5(var graph: TConnectionVectorVector; var benchmarks: TBenchmarkVector);
var
    f: TextFile;
    line: string;
    split: TStringArray;
    benchmark: TBenchmark;
    connection: TConnection;
    source, destination: Cardinal;
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
                PushBenchmarkVector(benchmarks, benchmark)
            else
                Break;
        end
        else
        begin
            if TryStrToUInt(split[0], source) and TryStrToUInt(split[1], destination) and TryStrToFloat(split[2], connection.distance) then
            begin
                GrowConnectionVectorVector(graph, Max(source, destination) + 1);
                connection.destination := destination;
                PushConnectionVector(graph.p[source], connection);
                connection.destination := source;
                PushConnectionVector(graph.p[destination], connection);
            end
            else Break;
        end;
    end;

    Close(f);
end;

procedure SolveVer5(const graph: TConnectionVectorVector; const benchmarks: TBenchmarkVector);
var
    candidates: array of TCandidate;
    candidate_indices: array of Cardinal;
    candidates_size: Cardinal;
    benchmark_i, candidate_i, connection_i: Cardinal;
    candidate, new_candidate: TCandidate;
    int_distance: Cardinal;
    distance: Single;
begin
    SetLength(candidates, graph.size);
    SetLength(candidate_indices, graph.size);
    
    for benchmark_i := 0 to benchmarks.size - 1 do
    begin
        candidates_size := 0;
        for candidate_i := 0 to graph.size - 1 do
            candidate_indices[candidate_i] := Cardinal(-1);
        
        candidate.id := benchmarks.p[benchmark_i].source;
        candidate.int_distance := 0;
        candidate.distance := 0.0;
        PushIndexHeap(candidates, candidates_size, candidate_indices, candidate);
        
        int_distance := 0;
        distance := Infinity;
        
        while candidates_size > 0 do
        begin
            candidate := PopIndexHeap(candidates, candidates_size, candidate_indices);
            if candidate.id = benchmarks.p[benchmark_i].destination then 
            begin
                int_distance := candidate.int_distance;
                distance := candidate.distance;
                Break; 
            end;
            
            for connection_i := 0 to graph.p[candidate.id].size - 1 do
            begin
                new_candidate.id := graph.p[candidate.id].p[connection_i].destination;
                new_candidate.int_distance := candidate.int_distance + 1;
                new_candidate.distance := candidate.distance + graph.p[candidate.id].p[connection_i].distance;
                PushIndexHeap(candidates, candidates_size, candidate_indices, new_candidate);
            end;
        end;

        WriteLn(benchmarks.p[benchmark_i].destination, ' ', int_distance, ' ', distance);
    end;
end;

procedure MainVer5();
var
    graph: TConnectionVectorVector;
    benchmarks: TBenchmarkVector;
begin
    graph.size := 0;
    benchmarks.size := 0;
    ParseVer5(graph, benchmarks);
    SolveVer5(graph, benchmarks);
end;

begin
    MainVer5();
end.
