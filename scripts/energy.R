# Calculate photovoltaic energy in kWh

# Arguments
#   A: Solar panel area (m2)
#   H: Annual average solar radiation (kWh)
#   r: Panel yield (0-1) (default value: 0.2)
#   PR: Performance ratio (0-1) (default value: 0.75)
# Returns:
#   Energy produced in kWh
energy <- function(A, H, r = 0.2, PR = 0.75) {
    E <- A * r * H * PR
    if (r < 0 || r > 1) {
      stop("r must be between 0 and 1")
    }
    
    if (PR < 0 || PR > 1) {
      stop("PR must be between 0 and 1")
    }
    return(E)
}
