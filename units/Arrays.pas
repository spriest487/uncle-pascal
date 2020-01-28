uses System;

function QuickSortPartition of T(arr: array of T; left, right: Integer): Integer
where T is Comparable
begin
    var l := left;
    var r := right;

    let pivot := arr[l];
    var result := r;

    while true do begin
        while Comparable.Compare(arr[l], pivot) < 0 do begin
            l := l + 1;
        end;
        while Comparable.Compare(arr[r], pivot) > 0 do begin
            r := r - 1;
        end;

        if l < r then begin
            if Comparable.Compare(arr[l], arr[r]) = 0 then begin
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

function QuickSortRange of T(arr: array of T; left, right: Integer)
where T is Comparable
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

export function QuickSort of T(arr: array of T)
where T is Comparable
begin
    QuickSortRange(arr, 0, Length(arr) - 1);
end;
