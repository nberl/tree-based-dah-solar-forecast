#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double haversine_distance(NumericVector point1, NumericVector point2) {
  double lon1 = point1[0];
  double lat1 = point1[1];
  double lon2 = point2[0];
  double lat2 = point2[1];
  
  int radius = 6371000; 
  
  double phi1 = lat1 * M_PI / 180;
  double phi2 = lat2 * M_PI / 180;
  double delta_phi = phi2 - phi1;
  double delta_lambda = (lon2 - lon1) * M_PI / 180;
  
  double hav_theta = pow(sin(delta_phi / 2), 2) + cos(phi1) * cos(phi2) * pow(sin(delta_lambda / 2), 2);
  
  float distance = (int)(2 * radius * asin(sqrt(hav_theta)) * 100 + .5); 
  
  return (double)(distance / 100);
  
}

// [[Rcpp::export]]
double objective_function(
    IntegerVector clusters,
    NumericVector distances,
    NumericVector weights) {
  
  int n_cluster = max(clusters) + 1;
  int n_obs = distances.size();
  NumericVector weight_k(n_cluster); 
  double objective = 0.0;
  
  for (int k = 0; k < n_cluster; k++) {
    for (int i = 0; i < n_obs; i++) { 
      if (clusters[i] == k) {
        weight_k[k] += weights[i];
      }      
    }
  } 
  
  for (int i = 0; i < n_obs; i++) {
    objective += pow(distances[i], 2) * weights[i] / weight_k[clusters[i]];
  }
  
  // Rcout << "min objective: " << objective << std::endl;
  
  return objective;
  
}

// [[Rcpp::export]]
List weighted_kmeans(
    DataFrame df,
    int n_cluster,
    int n_start,
    int max_iter,
    Nullable<CharacterVector> weight_col_name = R_NilValue,
    Nullable<NumericVector> seed = R_NilValue) {
  
  // set seed if given as input
  if (seed.isNotNull()) { 
    NumericVector seed_(seed);
    
    Rcout << "seed is set to : " << seed_[0] << std::endl;
    Rcpp::Environment base_env("package:base");
    Rcpp::Function set_seed_r = base_env["set.seed"];
    set_seed_r(std::floor(std::fabs(seed_[0])));
  }
  
  int n_rows = df.nrows();
  NumericVector weights(n_rows);
  
  // set weight variable, if column is given, otherwise set all weights to 1
  if (weight_col_name.isNotNull()) {
    CharacterVector weight_col_name_(weight_col_name);
    weights = df[std::string(weight_col_name_[0])];
  } else {
    std::fill(weights.begin(), weights.end(), 1);
  }
  
  // initialize
  NumericVector longitude = df["longitude"];
  NumericVector latitude = df["latitude"];
  NumericMatrix df_coord;
  df_coord = Rcpp::cbind(longitude, latitude);
  NumericMatrix df_centroid;
  List list_clusters(n_start);
  List list_centroids(n_start);
  NumericVector vect_objective(n_start);

  IntegerMatrix random_samples(n_start, n_cluster);
  for (int iter = 0; iter < n_start; iter++) {
    IntegerVector random_sample_iter = sample(n_rows, n_cluster);
    random_samples(iter, _) = clone(random_sample_iter) - 1;
  }
  
  for (int i_start = 0; i_start < n_start; i_start++) {
    Rcout << "random start round: " << i_start << std::endl;
    
    NumericMatrix distance_matrix(n_rows, n_cluster);
    IntegerVector clusters(n_rows);
    NumericVector distances(n_rows);
    IntegerVector previous_clusters(n_rows);

    // initial centriods
    IntegerVector init_centroids_index = random_samples(i_start, _);
    
    // compute distance between cities and initial centroids (haversine distance)
    for (int k = 0; k < n_cluster; k++) {
      for (int i = 0; i < n_rows; i++) {
        NumericVector loc_i = df_coord(i, _);
        NumericVector centroid_k = df_coord(init_centroids_index[k], _);
        distance_matrix(i, k) = haversine_distance(loc_i, centroid_k);
      }
    }
    
    // initial cluster assignment 
    for (int i = 0; i < n_rows; i++) {
      clusters[i] = which_min(distance_matrix(i, _));
    }
    
    IntegerVector current_clusters = clone(clusters);  

    int count_iter = 0;
    while (!is_true(all(previous_clusters == current_clusters)) && (count_iter < max_iter)) {
      count_iter += 1;
      previous_clusters = clone(current_clusters);
      
      // calculate centroids (weighted average)
      NumericVector centroid_lon(n_cluster);
      NumericVector centroid_lat(n_cluster);
      for (int k = 0; k < n_cluster; k++) {
        double numerator_k_lon = 0.0;
        double numerator_k_lat = 0.0;
        double denominator_k = 0.0;
        for (int i = 0; i < n_rows; i++) {
          if (previous_clusters[i] == k) {
            numerator_k_lon += df_coord(i, 0) * weights[i];
            numerator_k_lat += df_coord(i, 1) * weights[i];
            denominator_k += weights[i];  
          }
        }
        centroid_lon[k] = numerator_k_lon / denominator_k;
        centroid_lat[k] = numerator_k_lat / denominator_k;
      }
      
      df_centroid = Rcpp::cbind(centroid_lon, centroid_lat);
      
      // compute distance between locations and centroids
      for (int k = 0; k < n_cluster; k++) {
        for (int i = 0; i < n_rows; i++) {
          NumericVector loc_i = df_coord(i, _);
          NumericVector centroid_k = df_centroid(k, _);
          distance_matrix(i, k) = haversine_distance(loc_i, centroid_k);
        }
      }
      
      // update cluster assignment for each location
      for (int i = 0; i < n_rows; i++) {
        clusters[i] = which_min(distance_matrix(i, _));
        distances[i] = distance_matrix(i, clusters[i]);
      }
      // update new_cluster
      current_clusters = clone(clusters);
    }

    if (count_iter == max_iter) {
      Rcout << "not converged max iter reached: " << max_iter << std::endl;
    } else {
      Rcout << "converged after: " << count_iter << " iterations" << std::endl;
    }

    // store results of iteration
    list_clusters[i_start] = clone(current_clusters) + 1;
    list_centroids[i_start] = clone(df_centroid);
    vect_objective[i_start] = objective_function(clusters, distances, weights);
  }
  
  List out = List::create(_["clusters"] = list_clusters[which_min(vect_objective)] , 
                          _["centroids"] = list_centroids[which_min(vect_objective)]);
  
  return out;
  
}