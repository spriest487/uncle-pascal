uses System;

function QuickSortPartition(arr: array of String; left, right: Integer): Integer
begin
    var l := left;
    var r := right;

    let pivot := arr[l];
    var result := r;

    while true do begin
        while CompareStr(arr[l], pivot) < 0 do begin
            l := l + 1;
        end;
        while CompareStr(arr[r], pivot) > 0 do begin
            r := r - 1;
        end;

        if l < r then begin
            if CompareStr(arr[l], arr[r]) = 0 then begin
                exit r;
            end;

            let temp := arr[l];
            arr[l] := arr[r];
            arr[r] := temp;
        end
        else begin
            exit r;
        end;
    end;

    result
end;

function QuickSortRange(arr: array of String; left, right: Integer)
begin
    if left < right then begin
        let pivot := QuickSortPartition(arr, left, right);

        if pivot > 1 then begin
            QuickSortRange(arr, left, pivot - 1);
        end;
        if pivot + 1 < right then begin
            QuickSortRange(arr, pivot + 1, right);
        end;
    end;
end;

export function QuickSort(arr: array of String)
begin
    QuickSortRange(arr, 0, Length of String(arr) - 1);
end;