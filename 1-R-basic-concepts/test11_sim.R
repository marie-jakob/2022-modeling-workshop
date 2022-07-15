conf = list(
  name = "ten-sets_500",

    # Experiment setup:
    N = c(500), # number of trials/condition, may be a list
    Nsubj = 10,  # number of subjects/different parameter sets
  
#Params from Lerche et al (2016)
    a = c(0.5, 2),  # boundary separation
    t0 = c(0.2, 0.5),  # non-decision time (seconds)
    st0 = c(0.0, 0.2),  # variability in non-decision time (seconds), uniform distribution form t0-st0/2 to t0 + st0/2
    w = c(0.3, 0.7),  # bias (relative)
    sw = c(0.0, 0.5),  # variability in bias (relative), uniform distribution
    v = c(-4, 4),  # drift rates (for different conditions)
    sv = c(0, 1),  # variability in drift rate, normal distribution
    
    output_dir = "fits/test11"
  
)