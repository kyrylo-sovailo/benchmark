#!/usr/bin/env perl

use strict;
use warnings;
use List::Util qw(max);
use IO::File;

sub indexed_heap_push
{
    my ($data_ref, $indices_ref, $element) = @_;
    my $index = $indices_ref->[$element->{id}];
    if ($index == -1)
    {
        $index = scalar(@$data_ref);
        push @$data_ref, $element;
    }
    elsif ($index == -2)
    {
        return;
    }
    else
    {
        if ($element->{distance} >= $data_ref->[$index]->{distance})
        {
            return;
        }
    }

    while (1)
    {
        my $parent_exists = $index > 0;
        my $index_moved = 0;
        if ($parent_exists)
        {
            my $parent_index = int(($index - 1) / 2);
            if ($element->{distance} < $data_ref->[$parent_index]->{distance})
            {
                $data_ref->[$index] = $data_ref->[$parent_index];
                $indices_ref->[$data_ref->[$index]->{id}] = $index;
                $index = $parent_index;
                $index_moved = 1;
            }
        }
        if (!$index_moved)
        {
            $data_ref->[$index] = $element;
            $indices_ref->[$element->{id}] = $index;
            last;
        }
    }
}

sub indexed_heap_pop
{
    my ($data_ref, $indices_ref) = @_;
    my $top = $data_ref->[0];
    $indices_ref->[$top->{id}] = -2;
    my $back = $data_ref->[scalar(@$data_ref) - 1];
    pop @$data_ref;
    if (!@$data_ref) { return $top; } #If the front is the back, the algorithm no longer works

    my $index = 0;
    while (1)
    {
        my $left_index = 2 * $index + 1;
        my $right_index = 2 * $index + 2;
        my $left_exists = $left_index < scalar(@$data_ref);
        my $right_exists = $right_index < scalar(@$data_ref);

        my $index_moved = 0;
        if ($left_exists || $right_exists)
        {
            my $next_index;
            if ($left_exists && $right_exists)
            {
                if ($data_ref->[$left_index]->{distance} < $data_ref->[$right_index]->{distance})
                {
                    $next_index = $left_index;
                }
                else
                {
                    $next_index = $right_index;
                }
            }
            else
            {
                $next_index = $left_index;
            }

            if ($data_ref->[$next_index]->{distance} < $back->{distance})
            {
                $data_ref->[$index] = $data_ref->[$next_index];
                $indices_ref->[$data_ref->[$index]->{id}] = $index;
                $index = $next_index;
                $index_moved = 1;
            }
        }

        if (!$index_moved)
        {
            $data_ref->[$index] = $back;
            $indices_ref->[$back->{id}] = $index;
            last;
        }
    }

    return $top;
}

sub parse_ver2
{
    my @graph;
    my @benchmarks;
    my $read_benchmarks = 0;

    my $file = IO::File->new("dijkstra.txt", "r") or die "Cannot open file 'dijkstra.txt': $!";

    while (my $line = $file->getline)
    {
        chomp $line;
        my @split = grep { length } split(/\s+/, $line);
        my $split_length = @split;
        if ($split_length == 0)
        {
            #Whitespace
        }
        elsif ($split_length == 1 && $split[0] eq "GRAPH")
        {
            $read_benchmarks = 0;
        }
        elsif ($split_length == 1 && $split[0] eq "BENCHMARK")
        {
            $read_benchmarks = 1;
        }
        elsif ($read_benchmarks)
        {
            if ($split_length != 2) { last; } #Error
            my $source = $split[0];
            my $destination = $split[1];
            push @benchmarks, { source => $source, destination => $destination };
        }
        else
        {
            if ($split_length != 3) { last; } #Error
            my $source = $split[0];
            my $destination = $split[1];
            my $distance = $split[2];
            my $max_index = max($source, $destination);
            while (scalar(@graph) <= $max_index)
            {
                push @graph, {};
            }
            $graph[$source]->{$destination} = $distance;
            $graph[$destination]->{$source} = $distance;
        }
    }
    $file->close;
    return (\@graph, \@benchmarks);
}

sub solve_ver4
{
    my ($graph_ref, $benchmarks_ref) = @_;

    foreach my $benchmark (@$benchmarks_ref)
    {
        my $source = $benchmark->{source};
        my $destination = $benchmark->{destination};
        my @candidates;
        my @indices = (-1) x scalar(@$graph_ref);

        indexed_heap_push(\@candidates, \@indices, { id => $source, int_distance => 0, distance => 0.0 });

        my $distance = 'inf';
        my $int_distance = 0;

        while (@candidates)
        {
            my $candidate = indexed_heap_pop(\@candidates, \@indices);
            if ($candidate->{id} == $destination)
            {
                $int_distance = $candidate->{int_distance};
                $distance = $candidate->{distance};
                last;
            }
            my $connections = $graph_ref->[$candidate->{id}];

            while (my ($destination, $distance) = each %$connections)
            {
                indexed_heap_push(\@candidates, \@indices,
                {
                    id => $destination,
                    int_distance => $candidate->{int_distance} + 1,
                    distance => $candidate->{distance} + $distance
                });
            }
        }
        printf "%d -> %d: %f (%d)\n", $source, $destination, $distance, $int_distance;
    }
}

sub main_ver4
{
    my ($graph, $benchmarks) = parse_ver2();
    solve_ver4($graph, $benchmarks);
}

main_ver4();
