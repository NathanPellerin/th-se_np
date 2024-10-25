#include <fstream>
#include <sstream>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;

/******************************************************************************
 *  spliting a string knowing the number of resulting string pieces
 */
std::vector<std::string> strsplit(std::string str, std::string sep, int n) {
  
  int sep_length = sep.length();
  std::vector<std::string> out(n);
  size_t p = 0;

  for (int i = 0; i < n; i++) {
    p = str.find(sep);
    out[i] = str.substr(0, p);
    str.erase(0, p + sep_length);
  }

  return out;
}

/******************************************************************************
 *  spliting a string not knowing the number of resulting string pieces
 */
// [[Rcpp::export]]
std::vector<std::string> strsplit_cpp(std::string str, std::string sep) {
  
  int sep_length = sep.length();
  std::vector<std::string> out;
  size_t p = 0;
  
  while ( ((p = str.find(sep)) != std::string::npos) ) {
    out.push_back( str.substr(0, p) );
    str.erase(0, p + sep_length);
  }
  
  out.push_back(str);

  return out;
}

/******************************************************************************
 *  check if first lines in file match what is expected
 */
// [[Rcpp::export]]
bool is_optas_file(std::string fp) {
  bool out = true;
  std::vector<std::string> expected_lines;
  expected_lines.push_back("@context");
  expected_lines.push_back("");
  expected_lines.push_back("@ancestors");
  expected_lines.push_back("");
  expected_lines.push_back("@filters");
  expected_lines.push_back("@attributes");
  expected_lines.push_back("");
  expected_lines.push_back("@types");
  expected_lines.push_back("");
  expected_lines.push_back("@data");
  int n_lines = expected_lines.size();
  std::string l = "";
  std::ifstream f;
  f.open(fp.c_str());
  if (f.is_open()) {
    for (int k = 0; k < n_lines; k++) {
      if (!getline(f, l)) {
        out = false;
      } else {
        if (expected_lines[k] != "") {
          if (l != expected_lines[k]) {
            out = false;
          }
        }
      }
    }
    f.close();
  } else {
    out = false;
  }
  return(out);
}

/******************************************************************************
 *  get the variables names of an optas result file
 */
std::vector<std::string> get_optas_variables(std::string fp) {
  std::vector<std::string> var_names;
  std::string l = "";
  std::ifstream f;
  f.open(fp.c_str());
  while (l != "@attributes") {
    getline(f, l);
  }
  getline(f, l);
  var_names = strsplit_cpp(l, "\t");
  f.close();
  return(var_names);
}

/******************************************************************************
 *  get first simulation: time vector and all the other columns
 */
Rcpp::List get_optas_first(std::string fp, int n_var) {
  Rcpp::List out(2);
  int n_tst;
  // get raw results
  std::vector<std::string> raw_lines;
  std::string l = "";
  std::ifstream f;
  f.open(fp.c_str());
  while (l != "@start") {
    getline(f, l);
  }
  getline(f, l);
  while (l != "@end") {
    raw_lines.push_back(l);
    getline(f, l);
  }
  f.close();
  n_tst = raw_lines.size();
  // format results
  std::vector<std::string> time(n_tst);
  Rcpp::NumericMatrix variables(n_tst, n_var - 2);
  std::vector<std::string> split_l(n_var - 1);
  for (int k = 0; k < n_tst; k++) {
    split_l = strsplit(raw_lines[k], "\t", n_var - 1);
    time[k] = split_l[0];
    for (int i = 0; i < n_var - 2; i++) {
      variables(k, i) = std::atof(split_l[i + 1].c_str());
    }
  }
  out[0] = time;
  out[1] = variables;
  return(out);
}

/******************************************************************************
 *  get simulation ids
 */
std::vector<int> get_optas_sim_ids(std::string fp) {
  std::vector<int> sim_ids; 
  bool is_next = false;
  std::string l = "";
  std::vector<std::string> split_l(2);
  std::ifstream f;
  f.open(fp.c_str());
  while (l != "@data") {
    getline(f, l);
  }
  getline(f, l);
  split_l = strsplit(l, "\t", 2);
  sim_ids.push_back(std::atoi(split_l[1].c_str()));
  while (getline(f, l)) {
    if (is_next) {
      split_l = strsplit(l, "\t", 2);
      sim_ids.push_back(std::atoi(split_l[1].c_str()));
      is_next = false;
    }
    if (l == "@end") {
      is_next = true;
    }
  }
  f.close();
  return(sim_ids);
}


/******************************************************************************
 *  get information on the content of an optas result file
 */
// [[Rcpp::export]]
Rcpp::List get_optas_template(std::string fp) {
  // resulting List that contains 7 elements
  Rcpp::List out(7);
  int n_var = 0; // number of variables
  int n_tst = 0; // number of time steps
  int n_sim = 0; // number of simulations
  std::vector<std::string> var_names; // variables names
  std::vector<int> sim_ids; // simulation ids
  
  // get the variables count and names
  var_names = get_optas_variables(fp);
  n_var = var_names.size();
  
  // get the first simulation
  Rcpp::List first_sim(2);
  first_sim = get_optas_first(fp, n_var);
  std::vector<std::string> time = first_sim[0];
  Rcpp::NumericMatrix variables = first_sim[1];
  n_tst = time.size();
  
  // get simulatiuon count and ids
  sim_ids = get_optas_sim_ids(fp);
  n_sim = sim_ids.size();
  
  // return results
  out[0] = n_var - 1;
  out[1] = n_tst;
  out[2] = n_sim;
  out[3] = var_names;
  out[4] = time;
  out[5] = sim_ids;
  out[6] = variables;
  return(out);
  
}

/******************************************************************************
 *  get all simulation of an optas result file (one at a time, given a column id)
 */
Rcpp::NumericMatrix get_optas_simulations(std::string fp, int col_id, int n_tst, int n_sim, bool verbose = true) {
  Rcpp::NumericMatrix sim(n_tst, n_sim);
  
  bool in_sim = false;
  int i = 0, j = 0;
  std::string l = "";
  std::vector<std::string> split_l(col_id + 1);
  std::ifstream f;
  f.open(fp.c_str());
  while(getline(f, l)) {
  
    if (l == "@end") {
      in_sim = false;
      j += 1;
      if (verbose) {
        Rcpp::Rcout << "\r[" << floor((float)j / (float)n_sim * 100.0) << "%] Reading simulation " << j << "/" << n_sim;
      }
    }
    if (in_sim) {
      split_l = strsplit(l, "\t", col_id + 1);
      sim(i, j) = std::atof(split_l[col_id].c_str());
      i += 1;
    }
    if (l == "@start") {
      in_sim = true;
      i = 0;
    }
    
  }
  if (verbose) {
    Rcpp::Rcout << "\r[100%] Reading simulation " << n_sim << "/" << n_sim << "\n";
  }
  f.close();
  return(sim);
}

/******************************************************************************
 *  main function to be used from R
 *  get the content of an optas result file: 
 *  > common data
 *  > individual simulations
 *  > variables names, sim ids, dimensions (variables, timesteps, simulations)
 */
// [[Rcpp::export]]
Rcpp::List get_optas(std::string fp, std::vector<int> col_ids, bool verbose = true) {
  int n_col = col_ids.size();
  Rcpp::List out(n_col + 1);
  Function message("message");
  std::stringstream ss;
  
  // check if file is an optas result file
  if(!is_optas_file(fp)) {
    stop("Error while trying to read the file!");
  }
  
  // get general information on file and common variables
  if (verbose) {
    message("Getting common data and general information on file ...");
  }
  Rcpp::List info = get_optas_template(fp);
  int n_var = info[0];
  int n_tst = info[1];
  int n_sim = info[2];
  std::vector<std::string> var_names = info[3];
  out[0] = info;
  
  // get simulation results
  for (int k = 0; k < n_col; k++) {
    if (col_ids[k] >= n_var) {
      ss << col_ids[k];
      stop(std::string("Column id ") + ss.str() + " is larger than the number of variables found.");
    }
    if (verbose) {
      message(std::string("Getting simulation results of variable '") + var_names[col_ids[k]] + "' ...");
    }
    out[k + 1] = get_optas_simulations(fp, col_ids[k], n_tst, n_sim, verbose);
  }
  
  return(out);
}

/*** R

*/
