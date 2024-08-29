#ifndef ISHETEROZYGOUS_H
#define ISHETEROZYGOUS_H

#include <bitset>
#include <armadillo>

std::bitset<8> toBits(unsigned char byte);
unsigned char toByte(std::bitset<8> bits);

arma::Col<double> isHeterozygous(const arma::field<arma::Cube<unsigned char>>& geno,
                                 const arma::Col<int>& lociPerChr,
                                 arma::uvec lociLoc, int nThreads);

#endif // SIMPLYBEE_H
