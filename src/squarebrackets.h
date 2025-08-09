

#ifndef SQUAREBRACKETS_H
#define SQUAREBRACKETS_H



#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \
  POINTER[INDEX] = REPLACEMENT; \
} while(0)


#define MACRO_DIM_2(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
  R_xlen_t i_parts1, i_parts2;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2;     \
        DOCODE;               \
  	 }	\
	 }	\
} while(0)






#define MACRO_DIM_3(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
  R_xlen_t i_parts1, i_parts2, i_parts3;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_4(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_5(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_6(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_7(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_8(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
SEXP ind8 = VECTOR_ELT(sub, 7);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
int len8 = Rf_length(ind8);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7, i_parts8;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdim[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7 + i_parts8;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_9(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
SEXP ind8 = VECTOR_ELT(sub, 7);	\
SEXP ind9 = VECTOR_ELT(sub, 8);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
int len8 = Rf_length(ind8);	\
int len9 = Rf_length(ind9);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
const int *pind9 = INTEGER_RO(ind9);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7, i_parts8, i_parts9;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdim[7] * (pind9[iter9] - 1);	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdim[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7 + i_parts8 + i_parts9;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_10(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
SEXP ind8 = VECTOR_ELT(sub, 7);	\
SEXP ind9 = VECTOR_ELT(sub, 8);	\
SEXP ind10 = VECTOR_ELT(sub, 9);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
int len8 = Rf_length(ind8);	\
int len9 = Rf_length(ind9);	\
int len10 = Rf_length(ind10);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
const int *pind9 = INTEGER_RO(ind9);	\
const int *pind10 = INTEGER_RO(ind10);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7, i_parts8, i_parts9, i_parts10;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdim[8] * (pind10[iter10] - 1);	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdim[7] * (pind9[iter9] - 1);	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdim[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7 + i_parts8 + i_parts9 + i_parts10;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_11(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
SEXP ind8 = VECTOR_ELT(sub, 7);	\
SEXP ind9 = VECTOR_ELT(sub, 8);	\
SEXP ind10 = VECTOR_ELT(sub, 9);	\
SEXP ind11 = VECTOR_ELT(sub, 10);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
int len8 = Rf_length(ind8);	\
int len9 = Rf_length(ind9);	\
int len10 = Rf_length(ind10);	\
int len11 = Rf_length(ind11);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
const int *pind9 = INTEGER_RO(ind9);	\
const int *pind10 = INTEGER_RO(ind10);	\
const int *pind11 = INTEGER_RO(ind11);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7, i_parts8, i_parts9, i_parts10, i_parts11;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdim[9] * (pind11[iter11] - 1);	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdim[8] * (pind10[iter10] - 1);	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdim[7] * (pind9[iter9] - 1);	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdim[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7 + i_parts8 + i_parts9 + i_parts10 + i_parts11;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_12(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
SEXP ind8 = VECTOR_ELT(sub, 7);	\
SEXP ind9 = VECTOR_ELT(sub, 8);	\
SEXP ind10 = VECTOR_ELT(sub, 9);	\
SEXP ind11 = VECTOR_ELT(sub, 10);	\
SEXP ind12 = VECTOR_ELT(sub, 11);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
int len8 = Rf_length(ind8);	\
int len9 = Rf_length(ind9);	\
int len10 = Rf_length(ind10);	\
int len11 = Rf_length(ind11);	\
int len12 = Rf_length(ind12);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
const int *pind9 = INTEGER_RO(ind9);	\
const int *pind10 = INTEGER_RO(ind10);	\
const int *pind11 = INTEGER_RO(ind11);	\
const int *pind12 = INTEGER_RO(ind12);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7, i_parts8, i_parts9, i_parts10, i_parts11, i_parts12;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdim[10] * (pind12[iter12] - 1);	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdim[9] * (pind11[iter11] - 1);	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdim[8] * (pind10[iter10] - 1);	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdim[7] * (pind9[iter9] - 1);	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdim[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7 + i_parts8 + i_parts9 + i_parts10 + i_parts11 + i_parts12;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_13(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
SEXP ind8 = VECTOR_ELT(sub, 7);	\
SEXP ind9 = VECTOR_ELT(sub, 8);	\
SEXP ind10 = VECTOR_ELT(sub, 9);	\
SEXP ind11 = VECTOR_ELT(sub, 10);	\
SEXP ind12 = VECTOR_ELT(sub, 11);	\
SEXP ind13 = VECTOR_ELT(sub, 12);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
int len8 = Rf_length(ind8);	\
int len9 = Rf_length(ind9);	\
int len10 = Rf_length(ind10);	\
int len11 = Rf_length(ind11);	\
int len12 = Rf_length(ind12);	\
int len13 = Rf_length(ind13);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
const int *pind9 = INTEGER_RO(ind9);	\
const int *pind10 = INTEGER_RO(ind10);	\
const int *pind11 = INTEGER_RO(ind11);	\
const int *pind12 = INTEGER_RO(ind12);	\
const int *pind13 = INTEGER_RO(ind13);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7, i_parts8, i_parts9, i_parts10, i_parts11, i_parts12, i_parts13;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter13 = 0; iter13 < len13; ++iter13) {	\
	i_parts13 = pdim[11] * (pind13[iter13] - 1);	\
for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdim[10] * (pind12[iter12] - 1);	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdim[9] * (pind11[iter11] - 1);	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdim[8] * (pind10[iter10] - 1);	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdim[7] * (pind9[iter9] - 1);	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdim[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7 + i_parts8 + i_parts9 + i_parts10 + i_parts11 + i_parts12 + i_parts13;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_14(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
SEXP ind8 = VECTOR_ELT(sub, 7);	\
SEXP ind9 = VECTOR_ELT(sub, 8);	\
SEXP ind10 = VECTOR_ELT(sub, 9);	\
SEXP ind11 = VECTOR_ELT(sub, 10);	\
SEXP ind12 = VECTOR_ELT(sub, 11);	\
SEXP ind13 = VECTOR_ELT(sub, 12);	\
SEXP ind14 = VECTOR_ELT(sub, 13);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
int len8 = Rf_length(ind8);	\
int len9 = Rf_length(ind9);	\
int len10 = Rf_length(ind10);	\
int len11 = Rf_length(ind11);	\
int len12 = Rf_length(ind12);	\
int len13 = Rf_length(ind13);	\
int len14 = Rf_length(ind14);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
const int *pind9 = INTEGER_RO(ind9);	\
const int *pind10 = INTEGER_RO(ind10);	\
const int *pind11 = INTEGER_RO(ind11);	\
const int *pind12 = INTEGER_RO(ind12);	\
const int *pind13 = INTEGER_RO(ind13);	\
const int *pind14 = INTEGER_RO(ind14);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7, i_parts8, i_parts9, i_parts10, i_parts11, i_parts12, i_parts13, i_parts14;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter14 = 0; iter14 < len14; ++iter14) {	\
	i_parts14 = pdim[12] * (pind14[iter14] - 1);	\
for(int iter13 = 0; iter13 < len13; ++iter13) {	\
	i_parts13 = pdim[11] * (pind13[iter13] - 1);	\
for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdim[10] * (pind12[iter12] - 1);	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdim[9] * (pind11[iter11] - 1);	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdim[8] * (pind10[iter10] - 1);	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdim[7] * (pind9[iter9] - 1);	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdim[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7 + i_parts8 + i_parts9 + i_parts10 + i_parts11 + i_parts12 + i_parts13 + i_parts14;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_15(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
SEXP ind8 = VECTOR_ELT(sub, 7);	\
SEXP ind9 = VECTOR_ELT(sub, 8);	\
SEXP ind10 = VECTOR_ELT(sub, 9);	\
SEXP ind11 = VECTOR_ELT(sub, 10);	\
SEXP ind12 = VECTOR_ELT(sub, 11);	\
SEXP ind13 = VECTOR_ELT(sub, 12);	\
SEXP ind14 = VECTOR_ELT(sub, 13);	\
SEXP ind15 = VECTOR_ELT(sub, 14);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
int len8 = Rf_length(ind8);	\
int len9 = Rf_length(ind9);	\
int len10 = Rf_length(ind10);	\
int len11 = Rf_length(ind11);	\
int len12 = Rf_length(ind12);	\
int len13 = Rf_length(ind13);	\
int len14 = Rf_length(ind14);	\
int len15 = Rf_length(ind15);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
const int *pind9 = INTEGER_RO(ind9);	\
const int *pind10 = INTEGER_RO(ind10);	\
const int *pind11 = INTEGER_RO(ind11);	\
const int *pind12 = INTEGER_RO(ind12);	\
const int *pind13 = INTEGER_RO(ind13);	\
const int *pind14 = INTEGER_RO(ind14);	\
const int *pind15 = INTEGER_RO(ind15);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7, i_parts8, i_parts9, i_parts10, i_parts11, i_parts12, i_parts13, i_parts14, i_parts15;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter15 = 0; iter15 < len15; ++iter15) {	\
	i_parts15 = pdim[13] * (pind15[iter15] - 1);	\
for(int iter14 = 0; iter14 < len14; ++iter14) {	\
	i_parts14 = pdim[12] * (pind14[iter14] - 1);	\
for(int iter13 = 0; iter13 < len13; ++iter13) {	\
	i_parts13 = pdim[11] * (pind13[iter13] - 1);	\
for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdim[10] * (pind12[iter12] - 1);	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdim[9] * (pind11[iter11] - 1);	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdim[8] * (pind10[iter10] - 1);	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdim[7] * (pind9[iter9] - 1);	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdim[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7 + i_parts8 + i_parts9 + i_parts10 + i_parts11 + i_parts12 + i_parts13 + i_parts14 + i_parts15;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)






#define MACRO_DIM_16(DOCODE) do {      \
  SEXP ind1 = VECTOR_ELT(sub, 0);	\
SEXP ind2 = VECTOR_ELT(sub, 1);	\
SEXP ind3 = VECTOR_ELT(sub, 2);	\
SEXP ind4 = VECTOR_ELT(sub, 3);	\
SEXP ind5 = VECTOR_ELT(sub, 4);	\
SEXP ind6 = VECTOR_ELT(sub, 5);	\
SEXP ind7 = VECTOR_ELT(sub, 6);	\
SEXP ind8 = VECTOR_ELT(sub, 7);	\
SEXP ind9 = VECTOR_ELT(sub, 8);	\
SEXP ind10 = VECTOR_ELT(sub, 9);	\
SEXP ind11 = VECTOR_ELT(sub, 10);	\
SEXP ind12 = VECTOR_ELT(sub, 11);	\
SEXP ind13 = VECTOR_ELT(sub, 12);	\
SEXP ind14 = VECTOR_ELT(sub, 13);	\
SEXP ind15 = VECTOR_ELT(sub, 14);	\
SEXP ind16 = VECTOR_ELT(sub, 15);	\
  int len1 = Rf_length(ind1);	\
int len2 = Rf_length(ind2);	\
int len3 = Rf_length(ind3);	\
int len4 = Rf_length(ind4);	\
int len5 = Rf_length(ind5);	\
int len6 = Rf_length(ind6);	\
int len7 = Rf_length(ind7);	\
int len8 = Rf_length(ind8);	\
int len9 = Rf_length(ind9);	\
int len10 = Rf_length(ind10);	\
int len11 = Rf_length(ind11);	\
int len12 = Rf_length(ind12);	\
int len13 = Rf_length(ind13);	\
int len14 = Rf_length(ind14);	\
int len15 = Rf_length(ind15);	\
int len16 = Rf_length(ind16);	\
  const int *pind1 = INTEGER_RO(ind1);	\
const int *pind2 = INTEGER_RO(ind2);	\
const int *pind3 = INTEGER_RO(ind3);	\
const int *pind4 = INTEGER_RO(ind4);	\
const int *pind5 = INTEGER_RO(ind5);	\
const int *pind6 = INTEGER_RO(ind6);	\
const int *pind7 = INTEGER_RO(ind7);	\
const int *pind8 = INTEGER_RO(ind8);	\
const int *pind9 = INTEGER_RO(ind9);	\
const int *pind10 = INTEGER_RO(ind10);	\
const int *pind11 = INTEGER_RO(ind11);	\
const int *pind12 = INTEGER_RO(ind12);	\
const int *pind13 = INTEGER_RO(ind13);	\
const int *pind14 = INTEGER_RO(ind14);	\
const int *pind15 = INTEGER_RO(ind15);	\
const int *pind16 = INTEGER_RO(ind16);	\
  R_xlen_t i_parts1, i_parts2, i_parts3, i_parts4, i_parts5, i_parts6, i_parts7, i_parts8, i_parts9, i_parts10, i_parts11, i_parts12, i_parts13, i_parts14, i_parts15, i_parts16;  \
  double *pdim;              \
  pdim = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter16 = 0; iter16 < len16; ++iter16) {	\
	i_parts16 = pdim[14] * (pind16[iter16] - 1);	\
for(int iter15 = 0; iter15 < len15; ++iter15) {	\
	i_parts15 = pdim[13] * (pind15[iter15] - 1);	\
for(int iter14 = 0; iter14 < len14; ++iter14) {	\
	i_parts14 = pdim[12] * (pind14[iter14] - 1);	\
for(int iter13 = 0; iter13 < len13; ++iter13) {	\
	i_parts13 = pdim[11] * (pind13[iter13] - 1);	\
for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdim[10] * (pind12[iter12] - 1);	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdim[9] * (pind11[iter11] - 1);	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdim[8] * (pind10[iter10] - 1);	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdim[7] * (pind9[iter9] - 1);	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdim[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdim[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdim[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdim[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdim[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdim[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdim[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1];	\
        flatind = i_parts1 + i_parts2 + i_parts3 + i_parts4 + i_parts5 + i_parts6 + i_parts7 + i_parts8 + i_parts9 + i_parts10 + i_parts11 + i_parts12 + i_parts13 + i_parts14 + i_parts15 + i_parts16;     \
        DOCODE;               \
  	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
	 }	\
} while(0)




#define MACRO_DIM_DOCALL(DOCODE) do {     \
  int ndims = Rf_length(sub);         \
                                          \
  switch(ndims) {       \
    case 2:                                       \
{                                                 \
  MACRO_DIM_2(DOCODE);                           \
  break;                                          \
}                                                 \
case 3:                                       \
{                                                 \
  MACRO_DIM_3(DOCODE);                           \
  break;                                          \
}                                                 \
case 4:                                       \
{                                                 \
  MACRO_DIM_4(DOCODE);                           \
  break;                                          \
}                                                 \
case 5:                                       \
{                                                 \
  MACRO_DIM_5(DOCODE);                           \
  break;                                          \
}                                                 \
case 6:                                       \
{                                                 \
  MACRO_DIM_6(DOCODE);                           \
  break;                                          \
}                                                 \
case 7:                                       \
{                                                 \
  MACRO_DIM_7(DOCODE);                           \
  break;                                          \
}                                                 \
case 8:                                       \
{                                                 \
  MACRO_DIM_8(DOCODE);                           \
  break;                                          \
}                                                 \
case 9:                                       \
{                                                 \
  MACRO_DIM_9(DOCODE);                           \
  break;                                          \
}                                                 \
case 10:                                       \
{                                                 \
  MACRO_DIM_10(DOCODE);                           \
  break;                                          \
}                                                 \
case 11:                                       \
{                                                 \
  MACRO_DIM_11(DOCODE);                           \
  break;                                          \
}                                                 \
case 12:                                       \
{                                                 \
  MACRO_DIM_12(DOCODE);                           \
  break;                                          \
}                                                 \
case 13:                                       \
{                                                 \
  MACRO_DIM_13(DOCODE);                           \
  break;                                          \
}                                                 \
case 14:                                       \
{                                                 \
  MACRO_DIM_14(DOCODE);                           \
  break;                                          \
}                                                 \
case 15:                                       \
{                                                 \
  MACRO_DIM_15(DOCODE);                           \
  break;                                          \
}                                                 \
case 16:                                       \
{                                                 \
  MACRO_DIM_16(DOCODE);                           \
  break;                                          \
}                                                 \
       \
  }       \
} while(0)



#define MACRO_SUB2IND do {  \
  pout[counter] = flatind;   \
  counter++;              \
} while(0)



#define MACRO_SETARRAY0 do {  \
  x[flatind - 1] = rp[counter]; \
      counter++;  \
} while(0)



#define MACRO_SETARRAY1 do {  \
  x[flatind - 1] = rp[0]; \
} while(0)


#define MACRO_SLICEV_DO_NARM(DOCODE) do {	\
  	\
  switch(TYPEOF(y)) {	\
    	\
    case LGLSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = LOGICAL(y);	\
      const int pv = LOGICAL(v)[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if(py[i] != NA_LOGICAL && (py[i] == pv) == condition) {	\
          DOCODE;	\
        }	\
      }	\
      break;	\
    }	\
    case CPLXSXP:	\
    {	\
      bool condition = !invert[0];	\
      const Rcomplex *py = COMPLEX(y);	\
      const Rcomplex pv = COMPLEX(v)[0];	\
      bool checkNA;	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);	\
        if(!checkNA && (py[i].r == pv.r && py[i].i == pv.i) == condition) {	\
          DOCODE;	\
        }	\
      }	\
      break;	\
    }	\
    case RAWSXP :	\
    {	\
      bool condition = !invert[0];	\
      const Rbyte *py = RAW(y);	\
      const Rbyte pv = RAW(v)[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == pv) == condition) {	\
          DOCODE;	\
        }	\
      }	\
      break;	\
    }	\
    case INTSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = INTEGER(y);	\
      if(Rf_xlength(v) == 1) {	\
        int pv;	\
        pv = Rf_asInteger(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(py[i] != NA_INTEGER && (py[i] == pv) == condition) {	\
            DOCODE;	\
          }	\
        }	\
      }	\
      else if(Rf_xlength(v) == 2) {	\
        double *pv = REAL(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {	\
            DOCODE;	\
          }	\
        }	\
      }	\
      else {	\
        stop("improper length for `v`");	\
      }	\
      break;	\
    }	\
    case REALSXP:	\
    {	\
      bool condition = !invert[0];	\
      const double *py = REAL(y);	\
      if(Rf_xlength(v) == 1) {	\
        double pv;	\
        pv = Rf_asReal(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(!R_isnancpp(py[i]) && (py[i] == pv) == condition) {	\
            DOCODE;	\
          }	\
        }	\
      }	\
      else if(Rf_xlength(v) == 2) {	\
        double *pv = REAL(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {	\
            DOCODE;	\
          }	\
        }	\
      }	\
      else {	\
        stop("improper length for `v`");	\
      }	\
      	\
      break;	\
    }	\
    case STRSXP:	\
    {	\
      const SEXP *py = STRING_PTR_RO(y);	\
      if(Rf_length(v) == 1) {	\
        const SEXP *pv = STRING_PTR_RO(v);	\
        if(invert[0]) {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] != NA_STRING && py[i] != pv[0]) {	\
              DOCODE;	\
            }	\
          }	\
        }	\
        else {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == pv[0]) {	\
              DOCODE;	\
            }	\
          }	\
        }	\
      }	\
      if(Rf_length(v) > 1) {	\
        if(invert[0]) {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] != NA_STRING) {	\
              if(rcpp_count_stringmatches(py[i], v) == 0) {	\
                DOCODE;	\
              }	\
              	\
            }	\
          }	\
        }	\
        else {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(rcpp_count_stringmatches(py[i], v) > 0) {	\
              DOCODE;	\
            }	\
          }	\
        }	\
      }	\
      	\
      break;	\
    }	\
    default: stop("Unsupported type ");	\
  }	\
} while(0)


#define MACRO_SLICEV_DO_NAKEEP(DOCODE) do {	\
  switch(TYPEOF(y)) {	\
    	\
    case LGLSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = LOGICAL(y);	\
      const int pv = LOGICAL(v)[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if(py[i] == NA_LOGICAL || (py[i] == pv) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case CPLXSXP:	\
    {	\
      bool condition = !invert[0];	\
      const Rcomplex *py = COMPLEX(y);	\
      const Rcomplex pv = COMPLEX(v)[0];	\
      bool checkNA;	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);	\
        if(checkNA || (py[i].r == pv.r && py[i].i == pv.i) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case RAWSXP :	\
    {	\
      bool condition = !invert[0];	\
      const Rbyte *py = RAW(y);	\
      const Rbyte pv = RAW(v)[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == pv) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case INTSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = INTEGER(y);	\
      if(Rf_xlength(v) == 1) {	\
        int pv;	\
        pv = Rf_asInteger(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(py[i] == NA_INTEGER || (py[i] == pv) == condition) {	\
            DOCODE;	\
            	\
          }	\
        }	\
      }	\
      else if(Rf_xlength(v) == 2) {	\
        double *pv = REAL(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {	\
            DOCODE;	\
            	\
          }	\
        }	\
      }	\
      else {	\
        stop("improper length for `v`");	\
      }	\
      break;	\
    }	\
    case REALSXP:	\
    {	\
      bool condition = !invert[0];	\
      const double *py = REAL(y);	\
      if(Rf_xlength(v) == 1) {	\
        double pv;	\
        pv = Rf_asReal(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(R_isnancpp(py[i]) || (py[i] == pv) == condition) {	\
            DOCODE;	\
            	\
          }	\
        }	\
      }	\
      else if(Rf_xlength(v) == 2) {	\
        double *pv = REAL(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {	\
            DOCODE;	\
            	\
          }	\
        }	\
      }	\
      else {	\
        stop("improper length for `v`");	\
      }	\
      	\
      break;	\
    }	\
    case STRSXP:	\
    {	\
      const SEXP *py = STRING_PTR_RO(y);	\
      if(Rf_length(v) == 1) {	\
        const SEXP *pv = STRING_PTR_RO(v);	\
        if(invert[0]) {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == NA_STRING || py[i] != pv[0]) {	\
              DOCODE;	\
              	\
            }	\
          }	\
        }	\
        else {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == NA_STRING || py[i] == pv[0]) {	\
              DOCODE;	\
              	\
            }	\
          }	\
        }	\
      }	\
      if(Rf_length(v) > 1) {	\
        if(invert[0]) {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) == 0) {	\
              DOCODE;	\
              	\
            }	\
          }	\
        }	\
        else {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) > 0) {	\
              DOCODE;	\
              	\
            }	\
          }	\
        }	\
      }	\
      	\
      break;	\
    }	\
    default: stop("Unsupported type ");	\
  }	\
} while(0)


#define MACRO_SLICEV_DO_NAONLY(DOCODE) do {	\
  switch(TYPEOF(y)) {	\
    	\
    case LGLSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = LOGICAL(y);	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == NA_LOGICAL) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case INTSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = INTEGER(y);	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == NA_INTEGER) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case REALSXP:	\
    {	\
      bool condition = !invert[0];	\
      const double *py = REAL(y);	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if(R_isnancpp(py[i]) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case CPLXSXP:	\
    {	\
      const Rcomplex *py = COMPLEX(y);	\
      bool condition = !invert[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) == condition){	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case STRSXP:	\
    {	\
      const SEXP *py = STRING_PTR_RO(y);	\
      bool condition = !invert[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == NA_STRING) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case RAWSXP :	\
    {	\
      stop("NAs not defined for type `raw`");	\
    }	\
    default: stop("Unsupported type ");	\
  }	\
} while(0)


#define MACRO_SLICEV_DO(DOCODE) do {	\
  if(LogicalVector::is_na(na[0])) {	\
    MACRO_SLICEV_DO_NAONLY(DOCODE);	\
  }	\
  else if(na[0]) {	\
    MACRO_SLICEV_DO_NAKEEP(DOCODE);	\
  }	\
  else if(!na[0]) {	\
    MACRO_SLICEV_DO_NARM(DOCODE);	\
  }	\
  else {	\
    stop("unknow value for `na` given");	\
  }	\
} while(0)



#endif
