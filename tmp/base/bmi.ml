let bmi p =
    let (name, height, weight) = p in
    let bm = weight /. ( height *. height ) *. 10000.0 in
    let bmi_val = (float_of_int (int_of_float (100.0 *. bm +. 0.5))) /. 100.0 in
    let return = 
    if bmi_val < 18.0 
        then "やせ"
        else 
            if bmi_val < 25.0
                then "標準"
                else 
                    if bmi_val < 30.0
                        then "肥満"
                        else "高度肥満"
    in print_string (
        name ^ "さんは、BMI " ^ 
        (string_of_float bmi_val) ^ " で、" ^ 
        return ^ "です"
        );;

