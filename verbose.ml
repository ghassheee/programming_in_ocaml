
        (* random list self-made *)
        let nextrand seed =
            let a = 16807.0 and m =2147483647.0 in 
            let t = a *. seed in
            t -. m *. floor(t/.m) ;;
        let rec randlist_of_igarashi n seed tail = if n=0 then (seed,tail)
            else randlist_of_igarashi(n-1)(nextrand seed)(seed::tail) ;;
        

        (* bmi *)
        let bmi name h w    =
            let bm        = w /. (square h) *. 10000.0 in
            let bmi_value = (float_of_int(int_of_float(100.0*.bm+.0.5)))/.100.0 in
            let shape     =
                if bmi_value < 18.0 then "thin" else
                    if bmi_value < 25.0 then "moderate" else
                        if bmi_value < 30.0 then "fat" else "fattest" in
            print_string ( name ^ "'s BMI is " ^ 
                            string_of_float bmi_value ^ ", " ^ shape ^ ".");;
