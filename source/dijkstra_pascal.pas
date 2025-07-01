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
    index, parent_index: Cardinal;
    parent_exists, index_moved: Boolean;
begin
    index := indices[item.id];
    if index = Cardinal(-1) then
    begin
        index := size;
        Inc(size);
    end
    else if index = Cardinal(-2) then
        Exit
    else
    begin
        if item.distance >= data[index].distance then
            Exit;
    end;

    while True do
    begin
        parent_exists := index <> 0;
        index_moved := False;
        if parent_exists then
        begin
            parent_index := (index - 1) div 2;
            if item.distance < data[parent_index].distance then
            begin
                data[index] := data[parent_index];
                indices[data[index].id] := index;
                index := parent_index;
                index_moved := True;
            end;
        end;
        if not index_moved then
        begin
            data[index] := item;
            indices[item.id] := index;
            Break;
        end;
    end;
end;

function PopIndexHeap(var data: array of TCandidate; var size: Cardinal; var indices: array of Cardinal): TCandidate;
var
    index, left_index, right_index, next_index: Cardinal;
    left_exists, right_exists, index_moved: Boolean;
    back: TCandidate;
begin
    Dec(size);
    PopIndexHeap := data[0];
    indices[data[0].id] := Cardinal(-2);
    if size = 0 then
        Exit;

    back := data[size];
    index := 0;

    while True do
    begin
        left_index := 2 * index + 1;
        right_index := 2 * index + 2;
        left_exists := left_index <= size;
        right_exists := right_index <= size;

        index_moved := False;
        if left_exists or right_exists then
        begin
            if left_exists and right_exists then
            begin
                if data[left_index].distance < data[right_index].distance then
                    next_index := left_index
                else
                    next_index := right_index;
            end
            else
                next_index := left_index;

            if data[next_index].distance < back.distance then
            begin
                data[index] := data[next_index];
                indices[data[index].id] := index;
                index := next_index;
                index_moved := True;
            end;
        end;

        if not index_moved then
        begin
            data[index] := back;
            indices[back.id] := index;
            Break;
        end;
    end;
end;

procedure ParseVer5(var graph: TConnectionVectorVector; var benchmarks: TBenchmarkVector);
var
    f: TextFile;
    delimiters: set of Char;
    line, keyword_or_source: string;
    word_count: SizeInt;
    benchmark: TBenchmark;
    connection: TConnection;
    source, destination: Cardinal;
    read_benchmarks: Boolean;
begin
    Assign(f, 'dijkstra.txt');
    Reset(f);
    delimiters := [' ', #9];
    read_benchmarks := False;

    while not Eof(f) do
    begin
        ReadLn(f, line);
        word_count := WordCount(line, delimiters);

        if word_count = 0 then Continue;
        keyword_or_source := ExtractWord(1, line, delimiters);
        if (word_count = 1) and (keyword_or_source = 'GRAPH') then
            read_benchmarks := False
        else if (word_count = 1) and (keyword_or_source = 'BENCHMARK') then
            read_benchmarks := True
        else if read_benchmarks then
        begin
            if word_count <> 2 then Break; // Error
            if not TryStrToUInt(keyword_or_source, benchmark.source) then Break;
            if not TryStrToUInt(ExtractWord(2, line, delimiters), benchmark.destination) then Break;
            PushBenchmarkVector(benchmarks, benchmark);
        end
        else
        begin
            if word_count <> 3 then Break; // Error
            if not TryStrToUInt(keyword_or_source, source) then Break;
            if not TryStrToUInt(ExtractWord(2, line, delimiters), destination) then Break;
            if not TryStrToFloat(ExtractWord(3, line, delimiters), connection.distance) then Break;
            GrowConnectionVectorVector(graph, Max(source, destination) + 1);
            connection.destination := destination;
            PushConnectionVector(graph.p[source], connection);
            connection.destination := source;
            PushConnectionVector(graph.p[destination], connection);
        end;
    end;

    Close(f);
end;

procedure SolveVer5(const graph: TConnectionVectorVector; const benchmarks: TBenchmarkVector);
var
    //Queue of candidates
    candidates: array of TCandidate;
    candidate_indices: array of Cardinal;
    candidates_size: Cardinal;
    candidate: TCandidate;
    //Temporary values
    new_candidate: TCandidate;
    //Iteration
    benchmark_i, connection_i, candidate_i: Cardinal;
    benchmark: TBenchmark;
    connection: TConnection;
    //Result
    distance: Single;
    int_distance: Cardinal;
begin
    SetLength(candidates, graph.size);
    SetLength(candidate_indices, graph.size);
    
    if benchmarks.size > 0 then for benchmark_i := 0 to benchmarks.size - 1 do
    begin
        benchmark := benchmarks.p[benchmark_i];
        candidates_size := 0;
        if graph.size > 0 then for candidate_i := 0 to graph.size - 1 do
            candidate_indices[candidate_i] := Cardinal(-1);
        
        candidate.id := benchmark.source;
        candidate.int_distance := 0;
        candidate.distance := 0.0;
        PushIndexHeap(candidates, candidates_size, candidate_indices, candidate);
        
        distance := Infinity;
        int_distance := 0;
        
        while candidates_size > 0 do
        begin
            candidate := PopIndexHeap(candidates, candidates_size, candidate_indices);
            if candidate.id = benchmark.destination then 
            begin
                distance := candidate.distance;
                int_distance := candidate.int_distance;
                Break; 
            end;
            
            for connection_i := 0 to graph.p[candidate.id].size - 1 do
            begin
                connection := graph.p[candidate.id].p[connection_i];
                new_candidate.id := connection.destination;
                new_candidate.distance := candidate.distance + connection.distance;
                new_candidate.int_distance := candidate.int_distance + 1;
                PushIndexHeap(candidates, candidates_size, candidate_indices, new_candidate);
            end;
        end;

        Write(benchmark.source, ' -> ', benchmark.destination, ': ');
        WriteLn(FloatToStrF(distance, ffFixed, 100, 6), ' (', int_distance, ')');
    end;
end;

procedure MainVer5;
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
