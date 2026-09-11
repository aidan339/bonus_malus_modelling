library(tidyverse)
library('expm')
library(gridExtra)


# Create Transition Matrix

create_transition_matrix = function(lambda, nb_states) {
  
  P = matrix(0, nrow = nb_states, ncol = nb_states)
  
  p0 = dpois(0, lambda = lambda)
  p1 = dpois(1, lambda = lambda)
  p2plus = 1 - p0 - p1
  
  for (i in 1:nb_states) {
    if (i == 1) {
      P[i,i] = p0
      P[i, i + 1] = p1
      P[i, i + 2] = 1 - p0 - p1
    } else if (i == nb_states) {
      P[i, i-1] = p0
      P[i, i] = 1-p0
    } else if (i == nb_states - 1) {
      P[i, i - 1] = p0
      P[i, i + 1] = 1 - p0 
    } else {
      P[i,i-1] = p0
      P[i, i + 1] = p1
      P[i, i + 2] = 1 - p1 - p0
    }
  }
  return(P)
}


# Claim Rates

lambda_young = 0.3
lambda_old = 0.2

### ====================== Task 1 ====================== ###

###### Question 1 ########

# Current Scheme Transition Matrices and Discount
current_scheme_young = create_transition_matrix(lambda = lambda_young, nb_states = 5)
current_scheme_old = create_transition_matrix(lambda = lambda_old, nb_states = 5)

current_scheme_discount = data.frame(level = 0:4, discount = c(0.7, 0.8, 0.9, 1, 1.3))

write.csv(round(current_scheme_young, 3), file = 'current_scheme_1_transition_young.csv')
write.csv(round(current_scheme_old, 3), file = 'current_scheme_1_transition_old.csv')

# Option 1 Transition Matrices and Discount 
option_1_young = create_transition_matrix(lambda = lambda_young, nb_states = 3)
option_1_old = create_transition_matrix(lambda = lambda_old, nb_states = 3)

option_1_discount = data.frame(level = 0:2, discount = c(0.8, 0.9, 1))


write.csv(round(option_1_young, 3), file = 'scheme_1_transition_young.csv')
write.csv(round(option_1_old, 3), file = 'scheme_1_transition_old.csv')


# Option 2: Transition Matrices and Discount
option_2_young = create_transition_matrix(lambda = lambda_young, nb_states = 7)
option_2_old = create_transition_matrix(lambda = lambda_old, nb_states = 5)

write.csv(round(option_2_young, 3), file = 'scheme_2_transition_young.csv')
write.csv(round(option_2_old, 3), file = 'scheme_2_transition_old.csv')


option_2_discount_young = data.frame(level = 0:6, discount = c(0.6, 0.7, 0.8, 0.9, 1, 1.3, 1.6))
option_2_discount_old = current_scheme_discount


current_scheme_discount = current_scheme_discount %>%
  mutate(premium_young = 900 * discount, premium_old = 1400 * discount)

option_1_discount = option_1_discount %>%
  mutate(premium_young = 900 * discount, premium_old = 1400 * discount)

option_2_discount_old = option_2_discount_old %>%
  mutate(premium_old = 1400 * discount)

option_2_discount_young = option_2_discount_young %>%
  mutate(premium_young = 900 * discount)


###### Question 3 ########

policy_data = read.csv("AgeLevelData.csv")

is_young = function(age) {
  return(age >= 20 & age <= 25)
}


simulate_current_premium = function() {
  total_premium = 0
  for (i in 1:nrow(policy_data)) {
    age = policy_data$age[i]
    current_state = policy_data$ncdlevel26[i]
    row_idx = current_state + 1
    if (is_young(age)) {
      probs = current_scheme_young[row_idx, ]
      new_state = sample(0:4, size = 1, replace = TRUE, prob = probs)
      total_premium = total_premium + current_scheme_discount$premium_young[new_state + 1]

    } else {
      probs = current_scheme_old[row_idx, ]
      new_state = sample(0:4, size = 1, replace = TRUE, prob = probs)
      total_premium = total_premium + current_scheme_discount$premium_old[new_state + 1]
    }
  }
  return(total_premium)
}


simulate_task_1 = function(nsim = 2000) {
  out = numeric(nsim)
  for (i in 1:nsim) {
    out[i] = simulate_current_premium()
  }
  return(out)
}


simulate_ncd_state_distribution = function() {
  out = policy_data
  out['ncdlevel27'] = numeric(nrow(policy_data))
  for (i in 1:nrow(policy_data)) {
    age = policy_data$age[i]
    current_state = policy_data$ncdlevel26[i]
    row_idx = current_state + 1
    if (is_young(age)) {
      probs = current_scheme_young[row_idx, ]
      new_state = sample(0:4, size = 1, replace = TRUE, prob = probs)
    } else {
      probs = current_scheme_old[row_idx, ]
      new_state = sample(0:4, size = 1, replace = TRUE, prob = probs)
    }
    out[i, 'ncdlevel27'] = new_state
  }
  return(out)
}

distribution_27_current_scheme = simulate_ncd_state_distribution()


distribution_27_proportion = distribution_27_current_scheme %>%
  group_by(ncdlevel27) %>%
  summarise(proportion = n() / nrow(distribution_27_current_scheme)) %>%
  rename(ncdlevel = ncdlevel27) %>%
  mutate(group = "NCD level 27")

distribution_26_proportion = distribution_27_current_scheme %>%
  group_by(ncdlevel26) %>%
  summarise(proportion = n() / nrow(distribution_27_current_scheme)) %>%
  rename(ncdlevel = ncdlevel26) %>%
  mutate(group = "NCD level 26")

distribution_combined = bind_rows(
  distribution_26_proportion,
  distribution_27_proportion
)

ggplot(distribution_combined,
       aes(x = ncdlevel, y = proportion, fill = group)) +
  geom_col(
    position = "identity",
    alpha = 0.5
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Simulated Distribution of NCD Levels: 2026 vs 2027",
    x = "NCD level",
    y = "Proportion",
    fill = "Group"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )


simulated_premiums_task_1 = data.frame(x = simulate_task_1())

ggplot(simulated_premiums_task_1, aes(x=x)) + 
  geom_density(fill = 'steelblue', alpha = 0.4) + 
  labs(x = 'Premium',
       y = 'Density', 
       title = 'Distribution of Simulated Premium Premiums 2027 (Current Scheme)') +
  theme_minimal()



distr = simulated_premiums_task_1 %>%
  summarise(min = min(x), q25 = quantile(x, 0.25), median = median(x), 
            mean = mean(x), q75 = quantile(x, 0.75), max = max(x), std_dev = sd(x))

write.csv(distr, file = 'summary_stats_current_premium.csv')


### ====================== Task 2 ====================== ###

### Question 1 ###

current_scheme_long_run_young = (current_scheme_young %^% 10000)[1, ]
scheme_1_long_run_young = (option_1_young %^% 10000)[1, ]
scheme_2_long_run_young = (option_2_young %^% 10000)[1, ]

plt1 = ggplot(data.frame(ncd_level = 0:(length(current_scheme_long_run_young) - 1), long_run_proportion = current_scheme_long_run_young), aes(x = ncd_level, y = long_run_proportion)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Current Scheme Long-run Proportion (Young Drivers)",
    x = "NCD Level",
    y = "Long-run Proportion of Time"
  ) +
  theme_minimal() 

plt2 = ggplot(data.frame(ncd_level = 0:(length(scheme_1_long_run_young) - 1), long_run_proportion = scheme_1_long_run_young), aes(x = ncd_level, y = long_run_proportion)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Option 1 Long-run Proportion (Young Drivers)",
    x = "NCD Level",
    y = "Long-run Proportion of Time"
  ) +
  theme_minimal() 

plt3 = ggplot(data.frame(ncd_level = 0:(length(scheme_2_long_run_young) - 1), long_run_proportion = scheme_2_long_run_young), aes(x = ncd_level, y = long_run_proportion)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Option 2 Long-run Proportion (Young Drivers)",
    x = "NCD Level",
    y = "Long-run Proportion of Time"
  ) +
  theme_minimal() 


grid.arrange(plt1, plt2, plt3, layout_matrix = rbind(c(1,2), c(3,3)))

### Question 2 ###

age_summary = policy_data %>%
  mutate(young = is_young(age)) %>%
  summarise(young_percentage = sum(young)/nrow(policy_data)) 


generate_pi = function(lambda, nb_states) {
  P = create_transition_matrix(lambda = lambda, nb_states = nb_states)
  return((P %^% 10000)[1,])
}

generate_pi_lambda = function(nb_states) {
  out = data.frame(matrix(ncol = length(lambda_grid), nrow = nb_states))
  for (i in 1:length(lambda_grid)) {
    out[,i] = generate_pi(lambda = lambda_grid[i], nb_states = nb_states)
  }
  return(out)
}

lambda_grid = seq(0, 1.5, by = 0.05)


pi_lambda_current = generate_pi_lambda(nb_states = 5)

pi_lambda_scheme_1 = generate_pi_lambda(nb_states = 3)

pi_lambda_scheme_2_old = pi_lambda_current
pi_lambda_scheme_2_young = generate_pi_lambda(nb_states = 7)


pi_times_c_young_current = as.vector(as.vector(current_scheme_discount$premium_young) %*% as.matrix(pi_lambda_current))
pi_times_c_old_current = as.vector(as.vector(current_scheme_discount$premium_old) %*% as.matrix(pi_lambda_current))
pi_times_c_current = (pi_times_c_young_current + pi_times_c_old_current)/2

pi_dashed_current = numeric(length(pi_times_c_current) - 1)
for (i in 1:(length(pi_times_c_current)-1)) {
  pi_dashed_current[i] = (pi_times_c_current[i+1] - pi_times_c_current[i])/0.05
}

efficiency_current = ((lambda_grid[1:30] / pi_times_c_current[1:30]) * pi_dashed_current)[1:29]


pi_times_c_young_scheme_1 = as.vector(as.vector(option_1_discount$premium_young) %*% as.matrix(pi_lambda_scheme_1))
pi_times_c_old_scheme_1 = as.vector(as.vector(option_1_discount$premium_old) %*% as.matrix(pi_lambda_scheme_1))
pi_times_c_scheme_1 = (pi_times_c_old_scheme_1 + pi_times_c_young_scheme_1)/2

pi_times_c_young_scheme_2 = as.vector(as.vector(option_2_discount_young$premium_young) %*% as.matrix(pi_lambda_scheme_2_young))
pi_times_c_old_scheme_2 = as.vector(as.vector(option_2_discount_old$premium_old) %*% as.matrix(pi_lambda_scheme_2_old))
pi_times_c_scheme_2 = (pi_times_c_young_scheme_2 + pi_times_c_old_scheme_2)/2

pi_dashed_scheme_1 = numeric(length(pi_times_c_scheme_1) - 1)

for (i in 1:(length(pi_times_c_scheme_1)-1)) {
  pi_dashed_scheme_1[i] = (pi_times_c_scheme_1[i+1] - pi_times_c_scheme_1[i]) / 0.05
}

efficiency_scheme_1 = ((lambda_grid[1:30] / pi_times_c_scheme_1[1:30]) * pi_dashed_scheme_1)[1:29]

pi_dashed_scheme_2 = numeric(length(pi_times_c_scheme_2) - 1)

for (i in 1:(length(pi_times_c_scheme_2)-1)) {
  pi_dashed_scheme_2[i] = (pi_times_c_scheme_2[i+1] - pi_times_c_scheme_2[i]) / 0.05
}

efficiency_scheme_2 = ((lambda_grid[1:30] / pi_times_c_scheme_2[1:30]) * pi_dashed_scheme_2)[1:29]


efficiency_df = data.frame(
  lambda = lambda_grid[1:29],
  current_scheme = efficiency_current,
  scheme_1 = efficiency_scheme_1,
  scheme_2 = efficiency_scheme_2
)

efficiency_long = pivot_longer(
  efficiency_df,
  cols = c(current_scheme, scheme_1, scheme_2),
  names_to = "scheme",
  values_to = "efficiency"
)

ggplot(efficiency_long, aes(x = lambda, y = efficiency, colour = scheme)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() + 
  labs(x = 'Lambda',
       y = 'Loimaranta Efficiency',
       title = 'Loimaranta Efficiency of the Current and Proposed Schemes')



### Question 3 ###

#Current

long_run_profit_option_1_sim = function() {
  n = nrow(policy_data)
  
  long_run_young = (current_scheme_young %^% 10000)[1, ]
  long_run_old = (current_scheme_old %^% 10000)[1, ]

  young = is_young(policy_data$age)
  
  states = integer(n)
  states[young] = sample(0:4, sum(young), replace = TRUE, prob = long_run_young)
  states[!young] = sample(0:4, sum(!young), replace = TRUE, prob = long_run_old)
  
  premium <- numeric(n)
  premium[young] = current_scheme_discount$premium_young[states[young] + 1]
  premium[!young] = current_scheme_discount$premium_old[states[!young] + 1]
  
  expected_cost = ifelse(young, lambda_young, lambda_old) * 3200
  
  return(sum(premium) - sum(expected_cost))
}

long_run_profit_option_1 = function(nsim = 2000) {
  simulated_profits = rep(NA, nsim)
  for (i in 1:nsim) {
    simulated_profits[i] = long_run_profit_option_1_sim()
  }
  return(simulated_profits)
}

x = long_run_profit_option_1(nsim = 2000)
plot(density(x))
mean(x)


#Scheme 1

long_run_profit_scheme_1_sim = function() {
  n = nrow(policy_data)
  
  long_run_young = (option_1_young %^% 10000)[1, ]
  long_run_old = (option_1_old %^% 10000)[1, ]
  
  young = is_young(policy_data$age)
  
  states = integer(n)
  states[young] = sample(0:2, sum(young), replace = TRUE, prob = long_run_young)
  states[!young] = sample(0:2, sum(!young), replace = TRUE, prob = long_run_old)
  
  premium <- numeric(n)
  premium[young] = option_1_discount$premium_young[states[young] + 1]
  premium[!young] = option_1_discount$premium_old[states[!young] + 1]
  
  expected_cost = ifelse(young, lambda_young, lambda_old) * 3200
  
  return(sum(premium) - sum(expected_cost))
}


long_run_profit_scheme_1 = function(nsim = 2000) {
  simulated_profits = rep(NA, nsim)
  for (i in 1:nsim) {
    simulated_profits[i] = long_run_profit_scheme_1_sim()
  }
  return(simulated_profits)
}

#Scheme 2

long_run_profit_scheme_2_sim = function() {
  n = nrow(policy_data)
  
  long_run_young = (option_2_young %^% 10000)[1, ]
  long_run_old = (option_2_old %^% 10000)[1, ]
  
  young = is_young(policy_data$age)
  
  states = integer(n)
  states[young] = sample(0:6, sum(young), replace = TRUE, prob = long_run_young)
  states[!young] = sample(0:4, sum(!young), replace = TRUE, prob = long_run_old)
  
  premium <- numeric(n)
  premium[young] = option_2_discount_young$premium_young[states[young] + 1]
  premium[!young] = option_2_discount_old$premium_old[states[!young] + 1]
  
  expected_cost = ifelse(young, lambda_young, lambda_old) * 3200
  
  return(sum(premium) - sum(expected_cost))
}


long_run_profit_scheme_2 = function(nsim = 2000) {
  simulated_profits = rep(NA, nsim)
  for (i in 1:nsim) {
    simulated_profits[i] = long_run_profit_scheme_2_sim()
  }
  return(simulated_profits)
}


long_run_profits = data.frame(current = long_run_profit_option_1(), scheme_1 = long_run_profit_scheme_1(), scheme_2 = long_run_profit_scheme_2())
long_run_profits_long = long_run_profits %>%
  pivot_longer(everything(), 
               names_to = "scheme", 
               values_to = "profit")


ggplot(long_run_profits_long, aes(x = profit, fill = scheme, colour = scheme)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  labs(
    x = "Long-run profit",
    y = "Density",
    fill = "Scheme",
    colour = "Scheme",
    title = "Distribution of Long-Run Profits by Scheme"
  ) +
  theme_minimal()


long_run_profit_summary = as.data.frame(long_run_profits_long %>% 
  group_by(scheme) %>% 
  summarise(
    min = min(profit),
    Q1 = quantile(profit, 0.25),
    median = median(profit),
    Q3 = quantile(profit, 0.75),
    max = max(profit),
    mean = mean(profit),
    sd = sd(profit)
  )
)

write.csv(long_run_profit_summary, file = 'scheme_summary.csv')
