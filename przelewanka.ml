let przelewanka input_array =
    let glass =
        let input_list = Array.to_list input_array in
        let filter = List.filter
            (fun (x, _) -> x = 0) input_list in
        Array.of_list filter
    in
    let length = Array.length glass in
    let rec nwd a b =
        if b = 0 then a
        else nwd b (b mod a)
    in
    let nwdx =
        Array.fold_left (fun a (x, _) -> nwd (min x a) (max x a)) 0 glass in
    let test1 = Array.for_all (fun (_, y) -> y mod nwdx = 0) glass in
    let test2 = Array.exists (fun (x, y) -> (x = y || y = 0)) glass in
    let yt = Array.init n (fun i -> snd glass.(i)) in
    let hasht = Hashtbl.create n in
    let moves = ref 0 in
    let result = ref 0 in
    let q = Queue.create in
    let check_result =
        if Hashtbl.mem hasht yt then
        begin
            Queue.clear;
            result := Hashtbl.find hasht yt; end
            true
        else false
    in
    let hasht_update newstate =
        if not(Hashtbl.mem hasht newstate) then
            Hashtbl.add hasht newstate (!moves + 1);
            Queue.push newstate q;
    in
    let fill_empty state i =
        let statec = Array.copy state in
        statec.(i) <- fst glass.(i);
        hasht_update statec;
        statec.(i) <- 0;
        hasht_update statec;
    in
    let pour i j state =
        let statec = Array.copy state in
        if state.(j) + state.(i) <= fst glass.(j) then
        begin
            statec.(i) <- 0;
            statec.(j) <- state.(j) + state.(i);
            hasht_update statec; end
        else
        begin
            statec.(i) <- state.(i) - (fst glass.(j) - state.(j));
            statec.(j) <- fst glass.(j);
            hasht_update statec; end
    in
    if test1 && test2 then
    begin
        Hashtbl.add hasht (Array.make n 0) 0;
        Queue.push (Array.make n 0) q;
        while not(Queue.is_empty q) do
            let state = Queue.q pop in
            if not(check_result) then
                moves := Hashtbl.find hasht state;
                for i = 0 to (n - 1) do
                    fill_empty state i;
                    for j = 0 to (n - 1) do
                    if not(i = j) then pour i j state;
                    done              
                done
        done;
        if check_result then
            !result
        else -1 end
    else if glass = [||] then 0
        else -1;;