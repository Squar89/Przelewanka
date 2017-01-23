let przelewanka input_array =
    let glass =
        let input_list = Array.to_list input_array in
        let filter = List.filter
            (fun (x, _) -> not(x = 0)) input_list in
        Array.of_list filter
    in
    let length = Array.length glass in
    let rec nwd a b =
        if a = 0 then b
        else nwd (b mod a) a
    in
    let nwd_capacity =
        Array.fold_left (fun a (x, _) -> nwd (min x a) (max x a)) 0 glass in
    let desired_state = Array.init length (fun i -> snd glass.(i)) in
    let test1 = Array.for_all (fun (_, y) -> y mod nwd_capacity = 0) glass in
    let test2 = Array.exists (fun (x, y) -> (x = y || y = 0)) glass in
    let hash_table = Hashtbl.create 1000003 in
    let moves = ref (-1) in
    let q = Queue.create () in
    let check_result () =
        if Hashtbl.mem hash_table desired_state then
        begin
            Queue.clear q;
            true
        end
        else false
    in
    let hash_update new_state =
        if not(Hashtbl.mem hash_table new_state) then
        begin
            Hashtbl.add hash_table new_state (!moves + 1);
            Queue.push new_state q
        end
    in
    let fill_empty state i =
        let state_fill = Array.copy state in
        let state_empty = Array.copy state in
        state_fill.(i) <- fst glass.(i);
        hash_update state_fill;
        state_empty.(i) <- 0;
        hash_update state_empty
    in
    let pour state i j =
        let state_copy = Array.copy state in
        if state.(j) + state.(i) <= fst glass.(j) then
        begin
            state_copy.(i) <- 0;
            state_copy.(j) <- state.(j) + state.(i);
            hash_update state_copy
        end
        else
        begin
            state_copy.(i) <- state.(i) - (fst glass.(j) - state.(j));
            state_copy.(j) <- fst glass.(j);
            hash_update state_copy
        end
    in
    if test1 && test2 then
    begin
        hash_update (Array.make length 0);
        while not(Queue.is_empty q) do
            let state = Queue.pop q in
            if not(check_result ()) then
            begin
                moves := Hashtbl.find hash_table state;
                for i = 0 to (length - 1) do
                    fill_empty state i;
                    for j = 0 to (length - 1) do
                    if not(i = j) && not(state.(i) = 0) then pour state i j
                    done
                done
            end
        done;
        if Hashtbl.mem hash_table desired_state then
            Hashtbl.find hash_table desired_state
        else -1
    end
    else if glass = [||] then 0
        else -1;;