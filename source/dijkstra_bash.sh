#!/usr/bin/env bash
PATH=""
NEWLINE=$'\n'

float_round() {
    IFS="." read INT1 FRAC1 <<< "$1"
    FRAC1=${FRAC1:0:4}
    echo "$INT1.$FRAC1"
}

float_add() {
    IFS="." read INT1 FRAC1 <<< "$1"
    IFS="." read INT2 FRAC2 <<< "$2"
    if [ ${#FRAC2} -gt ${#FRAC1} ]; then
        for ((I = ${#FRAC2} - ${#FRAC1}; I > 0; I--)); do
            FRAC1="${FRAC1}0"
        done
    else
        for ((I = ${#FRAC1} - ${#FRAC2}; I > 0; I--)); do
            FRAC2="${FRAC2}0"
        done
    fi
    FRAC1="1$FRAC1"
    FRAC2="1$FRAC2"
    FRAC1=$(($FRAC1 + $FRAC2))
    if [ "${FRAC1:0:1}" -eq 3 ]; then
        INT1=$(($INT1 + $INT2 + 1))
    else
        INT1=$(($INT1 + $INT2))
    fi
    FRAC1="${FRAC1:1}"
    echo "$1 + $2 = $INT1.$FRAC1" >&2
    echo "$INT1.$FRAC1"
}

float_cmp() {
    IFS="." read INT1 FRAC1 <<< "$1"
    IFS="." read INT2 FRAC2 <<< "$2"
    if [ $INT1 -lt $INT2 ]; then
        echo "$1 <> $2 = -1" >&2
        echo -1
        return
    elif [ $INT1 -gt $INT2 ]; then
        echo "$1 <> $2 = 1" >&2
        echo 1
        return
    fi
    if [ ${#FRAC2} -gt ${#FRAC1} ]; then
        for ((I = ${#FRAC2} - ${#FRAC1}; I > 0; I--)); do
            FRAC1="${FRAC1}0"
        done
    else
        for ((I = ${#FRAC1} - ${#FRAC2}; I > 0; I--)); do
            FRAC2="${FRAC2}0"
        done
    fi
    FRAC1="1$FRAC1"
    FRAC2="1$FRAC2"
    if [ $FRAC1 -lt $FRAC2 ]; then
        echo "$1 <> $2 = -1" >&2
        echo -1
        return
    elif [ $FRAC1 -gt $FRAC2 ]; then
        echo "$1 <> $2 = 1" >&2
        echo 1
        return
    fi
    echo "$1 <> $2 = 0" >&2
    echo 0
}

float_candidate_cmp()
{
    read _ _ DISTANCE2 <<< "$2"
    float_cmp "$1" "${DISTANCE2}"
}

candidate_cmp()
{
    read _ _ DISTANCE1 <<< "$1"
    read _ _ DISTANCE2 <<< "$2"
    float_cmp "${DISTANCE1}" "${DISTANCE2}"
}

READ_BENCHMARKS=0
GRAPH=()
BENCHMARKS=()
while read LINE; do
    if [[ "$LINE" =~ ^[\s]*GRAPH[\s]*$ ]]; then
        READ_BENCHMARKS=0
    elif [[ "$LINE" =~ ^[\s]*BENCHMARK[\s]*$ ]]; then
        READ_BENCHMARKS=1
    elif [ $READ_BENCHMARKS -ne 0 ]; then
        read SOURCE DESTINATION <<< "$LINE"
        BENCHMARKS+=("$SOURCE $DESTINATION")
    else
        read SOURCE DESTINATION DISTANCE <<< "$LINE"
        DISTANCE=$(float_round "$DISTANCE")
        while [ ${#GRAPH[@]} -lt $SOURCE ]; do
            GRAPH+=("")
        done
        while [ ${#GRAPH[@]} -lt $DESTINATION ]; do
            GRAPH+=("")
        done
        GRAPH[$SOURCE]+="$DESTINATION $DISTANCE$NEWLINE"
        GRAPH[$DESTINATION]+="$SOURCE $DISTANCE$NEWLINE"
    fi
done <dijkstra.txt

for ((BENCHMARK_I=0; $BENCHMARK_I < ${#BENCHMARKS[@]}; BENCHMARK_I++)); do
    read SOURCE DESTINATION <<< "${BENCHMARKS[$BENCHMARK_I]}"
    INDICES=()
    for ((I=0; I < ${#GRAPH[@]}; I++)); do
        INDICES+=("")
    done
    INDICES[$SOURCE]=0
    CANDIDATES=("$SOURCE 0 0.0")
    DISTANCE="INF"
    INT_DISTANCE=0

    while [ ${#CANDIDATES[@]} -gt 0 ]; do
        # POP BEGIN
        read CANDIDATE_ID CANDIDATE_INT_DISTANCE CANDIDATE_DISTANCE <<< "${CANDIDATES[0]}"
        INDICES[$CANDIDATE_ID]="DEL"
        if [ ${#CANDIDATES[@]} -eq 1 ]; then
            CANDIDATES=()
        else
            BACK="${CANDIDATES[-1]}"
            INDEX=0
            while true; do
                LEFT_INDEX=$((2 * $INDEX + 1))
                RIGHT_INDEX=$((2 * $INDEX + 2))
                unset INDEX_MOVED
                if [ $LEFT_INDEX -lt ${#CANDIDATES[@]} ]; then
                    if [ $RIGHT_INDEX -lt ${#CANDIDATES[@]} ]; then
                        if [ $(candidate_cmp "${CANDIDATES[$LEFT_INDEX]}" "${CANDIDATES[$RIGHT_INDEX]}") -lt 0 ]; then
                            NEXT_INDEX=$LEFT_INDEX
                        else
                            NEXT_INDEX=$RIGHT_INDEX
                        fi
                    else
                        NEXT_INDEX=$LEFT_INDEX
                    fi
                    if [ $(candidate_cmp "${CANDIDATES[$NEXT_INDEX]}" "${BACK}") -lt 0 ]; then
                        CANDIDATES[$INDEX]="${CANDIDATES[$NEXT_INDEX]}"
                        read ID _ _ <<< "${CANDIDATES[$NEXT_INDEX]}"
                        INDICES[$ID]=$INDEX
                        INDEX=$NEXT_INDEX
                        INDEX_MOVED=1
                    fi
                fi
                if [ -z "$INDEX_MOVED" ]; then
                    CANDIDATES[$INDEX]="$BACK"
                    read ID _ _ <<< "$BACK"
                    INDICES[$ID]=$INDEX
                    unset CANDIDATES[-1]
                    break
                fi
            done
        fi
        # POP END

        if [ $CANDIDATE_ID -eq $DESTINATION ]; then
            INT_DISTANCE=$CANDIDATE_INT_DISTANCE
            DISTANCE=$CANDIDATE_DISTANCE
            break
        fi

        while read LINE; do
            [ -z "$LINE" ] && continue
            read NEW_ID NEW_DISTANCE <<< "$LINE"
            NEW_INT_DISTANCE=$(($CANDIDATE_INT_DISTANCE + 1))
            NEW_DISTANCE=$(float_add "$NEW_DISTANCE" "$CANDIDATE_DISTANCE")

            # PUSH BEGIN
            unset DO_PUSH
            INDEX="${INDICES[$NEW_ID]}"
            if [ -z "$INDEX" ]; then
                INDEX=${#CANDIDATES[@]}
                CANDIDATES+=("$NEW_ID $NEW_INT_DISTANCE $NEW_DISTANCE")
                DO_PUSH=1
            elif [ "$INDEX" != "DEL" ]; then
                if [ $(float_candidate_cmp "$NEW_DISTANCE" "${CANDIDATES[$INDEX]}") -lt 0 ]; then
                    DO_PUSH=1
                fi
            fi
            if [ -n "$DO_PUSH" ]; then
                while true; do
                    unset INDEX_MOVED
                    if [ $INDEX -gt 0 ]; then
                        PARENT_INDEX=$((($INDEX - 1) / 2))
                        if [ $(float_candidate_cmp "$NEW_DISTANCE" "${CANDIDATES[$PARENT_INDEX]}") -lt 0 ]; then
                            CANDIDATES[$INDEX]="${CANDIDATES[$PARENT_INDEX]}"
                            read ID _ _ <<< "${CANDIDATES[$PARENT_INDEX]}"
                            INDICES[$ID]=$INDEX
                            INDEX=$PARENT_INDEX
                            INDEX_MOVED=1
                        fi
                    fi
                    if [ -z "$INDEX_MOVED" ]; then
                        CANDIDATES[$INDEX]="$NEW_ID $NEW_INT_DISTANCE $NEW_DISTANCE"
                        INDICES[$NEW_ID]=$INDEX
                        break
                    fi
                done
            fi
            # PUSH END
        done <<< "${GRAPH[$CANDIDATE_ID]}"
    done

    echo "$SOURCE -> $DESTINATION: $DISTANCE ($INT_DISTANCE)"
done