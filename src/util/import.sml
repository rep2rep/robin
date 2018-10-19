val IMPORTED_ : string list ref = ref [];

fun import filename =
    let
        fun subDots str = String.implode
                                 (map (fn c => if c = #"." then #"/" else c)
                                      (String.explode str))
    in
        if (List.exists (fn s => s = filename) (!IMPORTED_))
        then () (* filename has already been imported *)
        else (
            IMPORTED_ := filename :: (!IMPORTED_);
            use (BASE^(subDots filename)^".sml")
        )
    end;
