

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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1);	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1);	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1);	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1);	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1);	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1);	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdcp[6] * (pind8[iter8] - 1);	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1) + i_parts8;	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdcp[7] * (pind9[iter9] - 1);	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdcp[6] * (pind8[iter8] - 1) + i_parts9;	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1) + i_parts8;	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdcp[8] * (pind10[iter10] - 1);	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdcp[7] * (pind9[iter9] - 1) + i_parts10;	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdcp[6] * (pind8[iter8] - 1) + i_parts9;	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1) + i_parts8;	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdcp[9] * (pind11[iter11] - 1);	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdcp[8] * (pind10[iter10] - 1) + i_parts11;	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdcp[7] * (pind9[iter9] - 1) + i_parts10;	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdcp[6] * (pind8[iter8] - 1) + i_parts9;	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1) + i_parts8;	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdcp[10] * (pind12[iter12] - 1);	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdcp[9] * (pind11[iter11] - 1) + i_parts12;	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdcp[8] * (pind10[iter10] - 1) + i_parts11;	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdcp[7] * (pind9[iter9] - 1) + i_parts10;	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdcp[6] * (pind8[iter8] - 1) + i_parts9;	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1) + i_parts8;	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter13 = 0; iter13 < len13; ++iter13) {	\
	i_parts13 = pdcp[11] * (pind13[iter13] - 1);	\
for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdcp[10] * (pind12[iter12] - 1) + i_parts13;	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdcp[9] * (pind11[iter11] - 1) + i_parts12;	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdcp[8] * (pind10[iter10] - 1) + i_parts11;	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdcp[7] * (pind9[iter9] - 1) + i_parts10;	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdcp[6] * (pind8[iter8] - 1) + i_parts9;	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1) + i_parts8;	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter14 = 0; iter14 < len14; ++iter14) {	\
	i_parts14 = pdcp[12] * (pind14[iter14] - 1);	\
for(int iter13 = 0; iter13 < len13; ++iter13) {	\
	i_parts13 = pdcp[11] * (pind13[iter13] - 1) + i_parts14;	\
for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdcp[10] * (pind12[iter12] - 1) + i_parts13;	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdcp[9] * (pind11[iter11] - 1) + i_parts12;	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdcp[8] * (pind10[iter10] - 1) + i_parts11;	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdcp[7] * (pind9[iter9] - 1) + i_parts10;	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdcp[6] * (pind8[iter8] - 1) + i_parts9;	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1) + i_parts8;	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter15 = 0; iter15 < len15; ++iter15) {	\
	i_parts15 = pdcp[13] * (pind15[iter15] - 1);	\
for(int iter14 = 0; iter14 < len14; ++iter14) {	\
	i_parts14 = pdcp[12] * (pind14[iter14] - 1) + i_parts15;	\
for(int iter13 = 0; iter13 < len13; ++iter13) {	\
	i_parts13 = pdcp[11] * (pind13[iter13] - 1) + i_parts14;	\
for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdcp[10] * (pind12[iter12] - 1) + i_parts13;	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdcp[9] * (pind11[iter11] - 1) + i_parts12;	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdcp[8] * (pind10[iter10] - 1) + i_parts11;	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdcp[7] * (pind9[iter9] - 1) + i_parts10;	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdcp[6] * (pind8[iter8] - 1) + i_parts9;	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1) + i_parts8;	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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
  double *pdcp;              \
  pdcp = REAL(dimcumprod); \
  R_xlen_t flatind = 0;           \
                              \
  for(int iter16 = 0; iter16 < len16; ++iter16) {	\
	i_parts16 = pdcp[14] * (pind16[iter16] - 1);	\
for(int iter15 = 0; iter15 < len15; ++iter15) {	\
	i_parts15 = pdcp[13] * (pind15[iter15] - 1) + i_parts16;	\
for(int iter14 = 0; iter14 < len14; ++iter14) {	\
	i_parts14 = pdcp[12] * (pind14[iter14] - 1) + i_parts15;	\
for(int iter13 = 0; iter13 < len13; ++iter13) {	\
	i_parts13 = pdcp[11] * (pind13[iter13] - 1) + i_parts14;	\
for(int iter12 = 0; iter12 < len12; ++iter12) {	\
	i_parts12 = pdcp[10] * (pind12[iter12] - 1) + i_parts13;	\
for(int iter11 = 0; iter11 < len11; ++iter11) {	\
	i_parts11 = pdcp[9] * (pind11[iter11] - 1) + i_parts12;	\
for(int iter10 = 0; iter10 < len10; ++iter10) {	\
	i_parts10 = pdcp[8] * (pind10[iter10] - 1) + i_parts11;	\
for(int iter9 = 0; iter9 < len9; ++iter9) {	\
	i_parts9 = pdcp[7] * (pind9[iter9] - 1) + i_parts10;	\
for(int iter8 = 0; iter8 < len8; ++iter8) {	\
	i_parts8 = pdcp[6] * (pind8[iter8] - 1) + i_parts9;	\
for(int iter7 = 0; iter7 < len7; ++iter7) {	\
	i_parts7 = pdcp[5] * (pind7[iter7] - 1) + i_parts8;	\
for(int iter6 = 0; iter6 < len6; ++iter6) {	\
	i_parts6 = pdcp[4] * (pind6[iter6] - 1) + i_parts7;	\
for(int iter5 = 0; iter5 < len5; ++iter5) {	\
	i_parts5 = pdcp[3] * (pind5[iter5] - 1) + i_parts6;	\
for(int iter4 = 0; iter4 < len4; ++iter4) {	\
	i_parts4 = pdcp[2] * (pind4[iter4] - 1) + i_parts5;	\
for(int iter3 = 0; iter3 < len3; ++iter3) {	\
	i_parts3 = pdcp[1] * (pind3[iter3] - 1) + i_parts4;	\
for(int iter2 = 0; iter2 < len2; ++iter2) {	\
	i_parts2 = pdcp[0] * (pind2[iter2] - 1) + i_parts3;	\
for(int iter1 = 0; iter1 < len1; ++iter1) {	\
	i_parts1 = pind1[iter1] + i_parts2;	\
        flatind = i_parts1;     \
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




inline int inline_count_stringmatches(SEXP y, SEXP v) {
  int n = Rf_length(v);
  const SEXP *pv = STRING_PTR_RO(v);
  for(int i = 0; i < n; ++i) {
    if((int)R_compute_identical(y, pv[i], 0)) {
      return 1;
    }
  }
  return 0;
}


#define MACRO_STRIDEV_BITS_WRITE(CONDITIONCODE) do {	\
    R_xlen_t _strv_n = endpos - startpos + 1; 	\
    R_len_t _strv_num_ints = _strv_n / 32;	\
    	\
    IntegerVector b32(_strv_num_ints + 1); 	\
    unsigned int* _strv_pb32 = (unsigned int*)INTEGER(b32); 	\
    	\
    R_xlen_t i = startpos; 	\
    	\
    /* MAIN LOOP */ 	\
    for (R_xlen_t _strv_int_idx = 0; _strv_int_idx < _strv_num_ints; ++_strv_int_idx) { 	\
        	\
        	\
        unsigned int _strv_reg = 0; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 0);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 1);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 2);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 3);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 4);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 5);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 6);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 7);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 8);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 9);  i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 10); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 11); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 12); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 13); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 14); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 15); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 16); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 17); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 18); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 19); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 20); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 21); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 22); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 23); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 24); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 25); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 26); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 27); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 28); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 29); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 30); i++; 	\
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 31); i++; 	\
        	\
        	\
        _strv_pb32[_strv_int_idx] = _strv_reg; 	\
    } 	\
    	\
    /* TAIL HANDLER */ 	\
    R_xlen_t _strv_rem = _strv_n % 32; 	\
    if (_strv_rem > 0) { 	\
        unsigned int _strv_reg = 0; 	\
        for (int _strv_b = 0; _strv_b < _strv_rem; ++_strv_b) { 	\
            if (((CONDITIONCODE)==condition) == 1) { 	\
                _strv_reg |= ((unsigned int)1 << _strv_b); 	\
            } 	\
            i++; 	\
        } 	\
        _strv_pb32[_strv_num_ints] = _strv_reg; 	\
    } 	\
    return b32; 	\
} while(0)

#define MACRO_STRIDEV_BITS_TRANSFER(DOCODE, STARTPOS, ENDPOS) do { 	\
    R_xlen_t _strv_n = (ENDPOS) - (STARTPOS) + 1; 	\
    int* _strv_native_ptr = INTEGER(b32); 	\
    unsigned int* _strv_pb32 = (unsigned int*)_strv_native_ptr; 	\
    	\
    R_xlen_t _strv_num_ints = _strv_n / 32; 	\
    R_xlen_t i = (STARTPOS); 	\
    	\
    /* MAIN LOOP */ 	\
    for (R_xlen_t _strv_int_idx = 0; _strv_int_idx < _strv_num_ints; ++_strv_int_idx) { 	\
        unsigned int _strv_current_int = _strv_pb32[_strv_int_idx]; 	\
        int _strv_bval; 	\
        	\
        _strv_bval = (_strv_current_int >> 0)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 1)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 2)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 3)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 4)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 5)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 6)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 7)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 8)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 9)  & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 10) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 11) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 12) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 13) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 14) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 15) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 16) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 17) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 18) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 19) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 20) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 21) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 22) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 23) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 24) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 25) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 26) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 27) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 28) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 29) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 30) & 1; if (_strv_bval) { DOCODE; } i++; 	\
        _strv_bval = (_strv_current_int >> 31) & 1; if (_strv_bval) { DOCODE; } i++; 	\
    } 	\
    	\
    /* TAIL HANDLER */ 	\
    R_xlen_t _strv_rem = _strv_n % 32; 	\
    if (_strv_rem > 0) { 	\
        unsigned int _strv_current_int = _strv_pb32[_strv_num_ints]; 	\
        int _strv_bval; 	\
        for (int _strv_b = 0; _strv_b < _strv_rem; ++_strv_b) { 	\
            _strv_bval = (_strv_current_int >> _strv_b) & 1; 	\
            if (_strv_bval) { 	\
                { DOCODE; } 	\
            } 	\
            i++;	\
        } 	\
    } 	\
} while(0)

#define MACRO_STRIDEV_PREP(CONDITIONCODE) do {	\
  	\
  R_xlen_t n = Rf_xlength(y);	\
  	\
  const NumericVector startpos = VECTOR_ELT(chunks, 0);	\
  const NumericVector endpos = VECTOR_ELT(chunks, 1);	\
  const int n_chunks = Rf_length(startpos);	\
  	\
  NumericVector first(n_chunks);	\
  NumericVector last(n_chunks);	\
  NumericVector count(n_chunks);	\
  NumericVector rnglen(n_chunks);	\
  	\
  for(int j = 0; j < n_chunks; ++j) {	\
    	\
    R_xlen_t startpos0 = startpos[j];	\
    R_xlen_t endpos0 = endpos[j];	\
    R_xlen_t first0 = -1;	\
    R_xlen_t last0 = -1;	\
    R_xlen_t count0 = 0;	\
    R_xlen_t rnglen0 = 0;	\
    	\
    	\
    for(R_xlen_t i = startpos0; i <= endpos0; ++i) {	\
      if((CONDITIONCODE) == condition) {	\
        first0 = i;	\
        last0 = i;	\
        break;	\
      }	\
    }	\
    	\
    if(first0 == endpos0) {	\
      count0 = 1;	\
    }	\
    if(first0 != -1 && first0 < endpos0) {	\
      for(R_xlen_t i = first0; i <= endpos0; ++i) {	\
        if((CONDITIONCODE) == condition) {	\
          count0++;	\
          last0 = i;	\
        }	\
      }	\
    }	\
    	\
    if(first0 > -1 && last0 > -1) {	\
      rnglen0 = last0 - first0 + 1;	\
    }	\
    	\
    first[j] = first0;	\
    last[j] = last0;	\
    count[j] = count0;	\
    rnglen[j] = rnglen0;	\
    	\
  }	\
  	\
  List out(4);	\
  out[0] = first;	\
  out[1] = last;	\
  out[2] = count;	\
  out[3] = rnglen;	\
  return out;	\
	\
} while(0)

#define MACRO_STRIDEV_POOL(CONDITIONCODE) do {	\
  	\
  const R_xlen_t first_total = prepvector[0];	\
  const R_xlen_t last_total = prepvector[1];	\
  const R_xlen_t count_total = prepvector[2];	\
  const R_xlen_t rnglen_total = prepvector[3];	\
  const int indexform = prepvector[4];	\
  	\
  const R_xlen_t n = Rf_xlength(y);	\
	\
  NumericVector first = preplist[0];	\
  NumericVector last = preplist[1];	\
  NumericVector count = preplist[2];	\
  NumericVector rnglen = preplist[3];	\
  const int n_chunks = Rf_length(first);	\
  List out(n_chunks);	\
  	\
  for(int j = 0; j < n_chunks; ++j) {	\
    const R_xlen_t current_count = count[j];	\
    const R_xlen_t current_rnglen = rnglen[j];	\
    	\
    if(current_count == current_rnglen) {	\
      out[j] = R_NilValue;	\
    }	\
    else if(current_count <= 2) {	\
      out[j] = R_NilValue;	\
    }	\
    else {	\
      out[j] = rcpp_stridev_bits_write(y, v, condition, na, startpos, endpos);	\
    }	\
  }	\
  	\
} while(0)

#define MACRO_STRIDEV_RAW(MACROCODE) do { \
  const Rbyte *py = RAW_RO(y);  \
  const Rbyte pv = RAW_RO(v)[0];  \
  if(LogicalVector::is_na(na[0])) { \
    stop("NAs not defined for type `raw`"); \
  } \
  else if(na[0]) {	\
    MACROCODE(  \
      (py[i] == pv) \
    ); \
  }	\
  else if(!na[0]) {  \
    MACROCODE(  \
      (py[i] == pv)  \
    );  \
  }	\
  else {	\
    stop("improper combination of `v` and `na` given");  \
  }	\
} while(0)

#define MACRO_STRIDEV_LGL(MACROCODE) do { \
  const int pv = LOGICAL_RO(v)[0]; \
  const int *py = LOGICAL_RO(y);  \
  if(LogicalVector::is_na(na[0])) { \
    MACROCODE(  \
      (py[i] == NA_LOGICAL)  \
    );  \
  } \
  else if(na[0]) {	\
    MACROCODE(  \
      (py[i] == NA_LOGICAL || (py[i] == pv)) \
    ); \
  }	\
  else if(!na[0]) {  \
    MACROCODE(  \
      (py[i] != NA_LOGICAL && (py[i] == pv))  \
    );  \
  }	\
  else {	\
    stop("improper combination of `v` and `na` given");  \
  }	\
} while(0)

#define MACRO_STRIDEV_INT(MACROCODE) do { \
  const int *py = INTEGER_RO(y);  \
  if(LogicalVector::is_na(na[0])) { \
    MACROCODE(  \
      (py[i] == NA_INTEGER)  \
    );  \
  } \
  else if(na[0] && Rf_length(v) == 1) {	\
    const double pv = REAL_RO(v)[0];  \
    MACROCODE(  \
      (py[i] == NA_INTEGER || (py[i] == pv)) \
    ); \
  }	\
  else if(na[0] && Rf_length(v) == 2) {	\
    const double *pv = REAL_RO(v);  \
    MACROCODE(  \
      (py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]))  \
    ); \
  }	\
  else if(!na[0] && Rf_length(v) == 1) {  \
    const double pv = REAL_RO(v)[0];  \
    MACROCODE(  \
      (py[i] != NA_INTEGER && (py[i] == pv))  \
    );  \
  }	\
  else if(!na[0] && Rf_length(v) == 2) { \
    const double *pv = REAL_RO(v);  \
    MACROCODE(  \
      (py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1])) \
    );  \
  }	\
  else {	\
    stop("improper combination of `v` and `na` given");  \
  }	\
} while(0)

#define MACRO_STRIDEV_REAL(MACROCODE) do {  \
  const double *py = REAL_RO(y);  \
  if(LogicalVector::is_na(na[0])) { \
    MACROCODE(  \
      (R_isnancpp(py[i])) \
    );  \
  } \
  else if(na[0] && Rf_length(v) == 1) {	\
    const double pv = REAL_RO(v)[0];  \
    MACROCODE(  \
      (R_isnancpp(py[i]) || (py[i] == pv))  \
    ); \
  }	\
  else if(na[0] && Rf_length(v) == 2) {	\
    const double *pv = REAL_RO(v);  \
    MACROCODE(  \
      (R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1])) \
    ); \
  }	\
  else if(!na[0] && Rf_length(v) == 1) {  \
    const double pv = REAL_RO(v)[0];  \
    MACROCODE(  \
      (!R_isnancpp(py[i]) && (py[i] == pv)) \
    );  \
  }	\
  else if(!na[0] && Rf_length(v) == 2) { \
    const double *pv = REAL_RO(v);  \
    MACROCODE(  \
      (!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]))  \
    );  \
  }	\
  else {	\
    stop("improper combination of `v` and `na` given");  \
  }	\
} while(0)

#define MACRO_STRIDEV_CPLX(MACROCODE) do {  \
  const Rcomplex *py = COMPLEX_RO(y); \
  if(LogicalVector::is_na(na[0])) { \
    MACROCODE(  \
      (R_isnancpp(py[i].r) || R_isnancpp(py[i].i))  \
    );  \
  } \
  else if(na[0]) {	\
    const Rcomplex pv = COMPLEX_RO(v)[0]; \
    MACROCODE(  \
      ((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) || (py[i].r == pv.r && py[i].i == pv.i)) \
    ); \
  }	\
  else if(!na[0]) {  \
    const Rcomplex pv = COMPLEX_RO(v)[0]; \
    MACROCODE(  \
      (!(R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) && (py[i].r == pv.r && py[i].i == pv.i))  \
    );  \
  }	\
  else {	\
    stop("improper combination of `v` and `na` given");  \
  }	\
} while(0)

#define MACRO_STRIDEV_STRING(MACROCODE) do {  \
  const SEXP *py = STRING_PTR_RO(y);  \
  const SEXP *pv = STRING_PTR_RO(v);  \
  if(LogicalVector::is_na(na[0])) { \
    MACROCODE(  \
      (py[i] == NA_STRING)  \
    );  \
  } \
  else if(na[0] && Rf_length(v) == 1) {	\
    MACROCODE(  \
      (py[i] == NA_STRING || (int)R_compute_identical(py[i], pv[0], 0)) \
    ); \
  }	\
  else if(na[0] && Rf_length(v) > 1) {	\
    MACROCODE(  \
      (py[i] == NA_STRING || inline_count_stringmatches(py[i], v))  \
    ); \
  }	\
  else if(!na[0] && Rf_length(v) == 1) {  \
    MACROCODE(  \
      (py[i] != NA_STRING && (int)R_compute_identical(py[i], pv[0], 0))  \
    );  \
  }	\
  else if(!na[0] && Rf_length(v) > 1) { \
    MACROCODE(  \
      (py[i] != NA_STRING && inline_count_stringmatches(py[i], v)) \
    );  \
  }	\
  else {	\
    stop("improper combination of `v` and `na` given");  \
  }	\
} while(0)

#define MACRO_STRIDEV_TYPESWITCH(MACROCODE) do {	\
  switch(TYPEOF(y)) {	\
    case RAWSXP:	\
    {	\
      MACRO_STRIDEV_RAW(MACROCODE);	\
      break;	\
    }	\
    case LGLSXP:	\
    {	\
      MACRO_STRIDEV_LGL(MACROCODE);	\
      break;	\
    }	\
    case INTSXP:	\
    {	\
      MACRO_STRIDEV_INT(MACROCODE);	\
      break;	\
    }	\
    case REALSXP:	\
    {	\
      MACRO_STRIDEV_REAL(MACROCODE);	\
      break;	\
    }	\
    case CPLXSXP:	\
    {	\
      MACRO_STRIDEV_CPLX(MACROCODE);	\
      break;	\
    }	\
    case STRSXP:	\
    {	\
      MACRO_STRIDEV_STRING(MACROCODE);	\
      break;	\
    }	\
    default:	\
    {	\
      stop("Unsupported type ");	\
    }	\
  }	\
} while(0)



#define MACRO_SLICE_SEQ_RANGE(STARTPOS, ENDPOS, DOCODE) do {  \
  for(R_xlen_t i = STARTPOS; i <= ENDPOS; ++i) {  \
    DOCODE; \
  } \
} while(0)


#define MACRO_SLICE_SEQ_FW(DOCODE) do {  \
  for(R_xlen_t i = start; i <= end; i += by) { \
    DOCODE; \
  } \
} while(0)


#define MACRO_SLICE_SEQ_BW(DOCODE) do {  \
  for(R_xlen_t i = start; i >= end; i -= by) { \
    DOCODE; \
  } \
} while(0)


#define MACRO_SLICE_SEQ_INV(DOCODE) do { \
  if(start > 0) {  \
    R_xlen_t startpos = 0;  \
    R_xlen_t endpos = start - 1; \
    MACRO_SLICE_SEQ_RANGE(  \
      startpos, endpos, \
      DOCODE  \
    );  \
  } \
  for(R_xlen_t j = start; j < end; j += by) {  \
    R_xlen_t startx = j + 1;  \
    for(R_xlen_t i = startx; i < (startx + by - 1); ++i) { \
      DOCODE; \
    } \
  } \
  if(end < (Rf_xlength(x) - 1)) {  \
    R_xlen_t startpos = end + 1; \
    R_xlen_t endpos = Rf_xlength(x) - 1;  \
    MACRO_SLICE_SEQ_RANGE(  \
      startpos, endpos, \
      DOCODE  \
    );  \
  } \
} while(0)


#define MACRO_SLICE_SEQ(DOCODE) do {  \
  const R_xlen_t start = rcpp_stride_get_Rxlent(stride, 0) - 1; \
  const R_xlen_t end = rcpp_stride_get_Rxlent(stride, 1) - 1; \
  const R_xlen_t by = rcpp_stride_get_Rxlent(stride, 2);  \
  const R_xlen_t len = rcpp_stride_get_Rxlent(stride, 5); \
  \
  if(len == 0) {  \
    break;  \
  } \
  else if(use < 0) { \
    MACRO_SLICE_SEQ_INV(DOCODE);  \
  } \
  else if(start <= end) { \
    MACRO_SLICE_SEQ_FW(DOCODE);  \
  } \
  else if(start > end) {  \
    MACRO_SLICE_SEQ_BW(DOCODE); \
  } \
  else {  \
    stop("unknown stride argument given");  \
  } \
} while(0)
  




#define MACRO_SLICE_PTRN_FW(DOCODE) do {  \
  R_xlen_t end_verysafe = end - ppattern[pattern_len - 1]; \
  R_xlen_t k = start; \
  R_xlen_t i; \
  for(; k <= end_verysafe; k += by) { \
    for(R_xlen_t j = 0; j < pattern_len; ++j) { \
      i = k + ppattern[j]; \
      DOCODE; \
    } \
  } \
  for (; k <= end; k += by) { \
    for (R_xlen_t j = 0; j < pattern_len; ++j) {  \
      i = k + ppattern[j]; \
      if (i > end) break; \
      DOCODE; \
    } \
  } \
} while(0)


#define MACRO_SLICE_PTRN_BW(DOCODE) do {  \
  R_xlen_t end_verysafe = end + ppattern[pattern_len - 1]; \
  R_xlen_t k = start; \
  R_xlen_t i; \
  for(; k >= end_verysafe; k -= by) { \
    for(R_xlen_t j = 0; j < pattern_len; ++j) { \
      i = k - ppattern[j]; \
      DOCODE; \
    } \
  } \
  for (; k >= end; k -= by) { \
    for (R_xlen_t j = 0; j < pattern_len; ++j) {  \
      i = k - ppattern[j]; \
      if (i < end) break; \
      DOCODE; \
    } \
  } \
} while(0)


#define MACRO_SLICE_PTRN_RANGE(STARTPOS, ENDPOS, DOCODE) do { \
  for(R_xlen_t i = STARTPOS; i <= ENDPOS; ++i) {  \
    DOCODE; \
  } \
} while(0)


#define MACRO_SLICE_PTRN_INV(DOCODE) do { \
  if(start > 0) {  \
    R_xlen_t startpos = 0;  \
    R_xlen_t endpos = start - 1; \
    MACRO_SLICE_PTRN_RANGE( \
      startpos, endpos, \
      DOCODE  \
    );  \
  } \
    \
  MACRO_SLICE_PTRN_FW(DOCODE);  \
    \
  if(end < (Rf_xlength(x) - 1)) {  \
    R_xlen_t startpos = end + 1; \
    R_xlen_t endpos = Rf_xlength(x) - 1;  \
    MACRO_SLICE_PTRN_RANGE( \
      startpos, endpos, \
      DOCODE; \
    );  \
  } \
} while(0)




#define MACRO_SLICE_PTRN(DOCODE) do {  \
  const R_xlen_t start = rcpp_stride_get_Rxlent(stride, 0) - 1; \
  const R_xlen_t end = rcpp_stride_get_Rxlent(stride, 1) - 1; \
  const R_xlen_t by = rcpp_stride_get_Rxlent(stride, 2);  \
  const R_xlen_t len = rcpp_stride_get_Rxlent(stride, 5);  \
  const SEXP pattern = rcpp_stride_get_pattern(stride);  \
  const int *ppattern = INTEGER_RO(pattern);  \
  const R_xlen_t pattern_len = Rf_xlength(pattern); \
  \
  if(len == 0) {  \
    break;  \
  } \
  else if(use < 0) { \
    MACRO_SLICE_PTRN_INV(DOCODE);  \
  } \
  else if(start <= end) { \
    MACRO_SLICE_PTRN_FW(DOCODE);  \
  } \
  else if(start > end) {  \
    MACRO_SLICE_PTRN_BW(DOCODE); \
  } \
  else {  \
    stop("unknown stride argument given");  \
  } \
} while(0)
  





#endif
