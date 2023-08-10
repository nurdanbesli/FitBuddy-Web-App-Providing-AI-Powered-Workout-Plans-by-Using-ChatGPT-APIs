#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double calculate_bmi_cpp(double weight, double height) {
  double bmi = weight / pow(height / 100.0, 2);
  return std::round(bmi * 10) / 10; // round to 1 decimal place
}

// [[Rcpp::export]]
double calculate_bmr_cpp(double weight, double height, int age, std::string gender) {
  double bmr;
  if (gender == "Male") {
    bmr = 88.36 + (13.4 * weight) + (4.8 * height) - (5.7 * age);
  } else {
    bmr = 447.6 + (9.2 * weight) + (3.1 * height) - (4.3 * age);
  }
  return std::round(bmr); // round to nearest whole number
}
