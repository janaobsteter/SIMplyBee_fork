#include "simplybee.h"
#include <RcppArmadillo.h>

std::bitset<8> toBits(unsigned char byte){
  return std::bitset<8>(byte);
}

//unsigned char toByte(std::bitset<8> bits){
  // return bits.to_ulong();
//}

/*
 * Genotype data is stored in a field of cubes.
 * The field has length equal to nChr
 * Each cube has dimensions nLoci/8 by ploidy by nInd
 * Output returned with dimensions nInd by nLoci
 */
// [[Rcpp::export]]
arma::Col<double> isHeterozygous(const arma::field<arma::Cube<unsigned char> >& geno,
                                 const arma::Col<int>& lociPerChr,
                                 arma::uvec lociLoc, int nThreads){
  // R to C++ index correction
  lociLoc -= 1;

  arma::uword nInd = geno(0).n_slices;
  arma::uword nChr = geno.n_elem;
  arma::uword ploidy = geno(0).n_cols;
  if(nInd < static_cast<arma::uword>(nThreads) ){
    nThreads = nInd;
  }
  arma::Mat<unsigned char> genotype(nInd,arma::sum(lociPerChr),arma::fill::zeros);
  int loc1;
  int loc2 = -1;
  for(arma::uword i=0; i<nChr; ++i){
    if(lociPerChr(i)>0){
      // Get loci locations
      loc1 = loc2+1;
      loc2 += lociPerChr(i);
      arma::uvec chrLociLoc = lociLoc(arma::span(loc1,loc2));
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(nThreads)
#endif
      for(arma::uword ind=0; ind<nInd; ++ind){
        std::bitset<8> workBits;
        arma::uword currentByte, newByte;
        for(arma::uword p=0; p<ploidy; ++p){
          currentByte = chrLociLoc(0)/8;
          workBits = toBits(geno(i)(currentByte,p,ind));
          genotype(ind,loc1) += (unsigned char) workBits[chrLociLoc(0)%8];
          for(arma::uword j=1; j<chrLociLoc.n_elem; ++j){
            newByte = chrLociLoc(j)/8;
            if(newByte != currentByte){
              currentByte = newByte;
              workBits = toBits(geno(i)(currentByte,p,ind));
            }
            genotype(ind,j+loc1) += (unsigned char) workBits[chrLociLoc(j)%8];
          }
        }
      }
    }
  }
  arma::Col<double> output(genotype.n_rows, arma::fill::zeros);

  for (arma::uword i = 0; i < genotype.n_rows; ++i) {
    // Check if any element in the row is equal to 1
    if (arma::any(genotype.row(i) == 1)) {
      output(i) = 1;
    }
  }
  return output;
}
