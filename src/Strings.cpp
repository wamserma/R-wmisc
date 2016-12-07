#include <Rcpp.h>
using namespace Rcpp;

// head and tail functions for Strings

// get first character of a String
//[[Rcpp::export]]
char strHead(std::string s) {
  return s[0];
}

// get first character of a String and convert to lowercase
//[[Rcpp::export]]
char strHeadLower(std::string s) {
  return tolower(s[0]);
}

// get tail of a String
//[[Rcpp::export]]
std::string strTail(std::string s) {
  if (!s.empty())
    s = &(s[1]);
  return s;
}

// get init of a String (a copy)
//[[Rcpp::export]]
std::string strTake(std::string s, int n) {
  s = s.substr(0,n);
  return s;
}

// get remainder of a String
//[[Rcpp::export]]
std::string strDrop(std::string s, int n) {
  if (!(s.length()<n))
    s = &(s[n]);
  else
    s = "";
  return s;
}
