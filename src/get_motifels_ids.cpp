// #include "get_motifels_ids.h"

// // [[Rcpp::export]]
// IntegerMatrix get_motifels_ids(int num_r, int num_c, int size, int shift) {
//
//   int nr_of_motifels = 0;
//   for (int i = 0; i < num_r; i = i + shift) {
//     for (int j = 0; j < num_c; j = j + shift) {
//       nr_of_motifels ++;
//     }
//   }
//
//   IntegerMatrix result(nr_of_motifels, 2);
//
//   int m = 0;
//   int m_row = 1;
//   int m_col = 1;
//
//   for (int j = 0; j < num_c; j = j + shift){
//     for (int i = 0; i < num_r; i = i + shift){
//       result(m, 0) = m_row;
//       result(m, 1) = m_col;
//       m++;
//       m_row++;
//     }
//     m_row = 1;
//     m_col++;
//   }
//   return result;
// }
