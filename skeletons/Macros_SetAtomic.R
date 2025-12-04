

macro_set_array <- "
#define MACRO_SET_ARRAY do { \\
  if(rp.length() == n) {  \\
    R_xlen_t counter = 0; \\
    MACRO_DIM_DOCALL(MACRO_SETARRAY0);  \\
  } \\
  else if(rp.length() == 1) { \\
    MACRO_DIM_DOCALL(MACRO_SETARRAY1);  \\
  } \\
  else stop(\"recycling not allowed\");
} while(0)
"

macro_set_vector0 <- "

#define MACRO_SET_VECTOR0 do {  \\
  px[flatind - 1] = prp[counter]; \\
      counter++;  \\
} while(0)

"

macro_set_vector1 <- "

#define MACRO_SET_VECTOR1 do {  \\
  px[flatind - 1] = prp[0]; \\
} while(0)

"


