#include "get_motifel_size.h"

// [[Rcpp::export]]
int get_motifel_size(int num_r, int num_c, int shift){
  int nr_of_motifels = 0;
  for (int i = 0; i < num_r; i = i + shift) {
    for (int j = 0; j < num_c; j = j + shift) {
      nr_of_motifels ++;
    }
  }
  return nr_of_motifels;
}

