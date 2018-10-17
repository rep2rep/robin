val IMPORTED_ : string list ref = ref [];

fun import filename =
    if (List.exists (fn s => s = filename) (!IMPORTED_))
    then () (* filename has already been imported *)
    else (
        IMPORTED_ := filename :: (!IMPORTED_);
        use (BASE^filename)
    );
