##%#########################################################################%##
#                                                                             #
#             Operations Analyst Code script - Zoja Manček Páli               #
#                              Started: 30.9.2023                             #
#                                                                             #
##%#########################################################################%##

#WD
setwd("~/") #erases previously set WDs
setwd("~/Desktop/Zoja Complete Repository/Operations Analyst Code - Application") #sets a new one
getwd()

#Libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(knitr)
library(plotly)
library(scales)
library(DT)
library(corrplot)


# 1. REVOPS & ANALYTICS ============================================================================
# CRM Funnel Analysis with Sample Data
set.seed(123)
crm_data <- data.frame(
  lead_id = 1:1000,
  source = sample(c("Website", "LinkedIn", "Referral", "Cold Outreach", "Event"), 1000, replace = TRUE),
  lead_date = sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), 1000, replace = TRUE),
  stage = sample(c("Lead", "MQL", "SQL", "Opportunity", "Closed Won", "Closed Lost"), 1000, 
                 replace = TRUE, prob = c(0.4, 0.25, 0.15, 0.1, 0.05, 0.05)),
  deal_value = ifelse(runif(1000) > 0.7, round(runif(1000, 5000, 50000), -2), NA),
  days_in_stage = sample(1:90, 1000, replace = TRUE)
)

# Funnel Conversion Analysis
funnel_analysis <- function(data) {
  stage_order <- c("Lead", "MQL", "SQL", "Opportunity", "Closed Won")
  
  # First, get the counts
  stage_counts <- data %>%
    filter(stage %in% stage_order) %>%
    count(stage, name = "count") %>%
    mutate(stage = factor(stage, levels = stage_order)) %>%
    arrange(stage)
  
  # Get the Lead count (first stage) for conversion calculations
  lead_count <- stage_counts$count[stage_counts$stage == "Lead"]
  
  # Calculate conversions properly
  funnel_metrics <- stage_counts %>%
    mutate(
      conversion_from_leads = round((count / lead_count) * 100, 1),
      stage_conversion = round((count / lag(count, default = lead_count)) * 100, 1)
    )
  
  return(funnel_metrics)
}

funnel_results <- funnel_analysis(crm_data)
print("CRM Funnel Conversion Rates:")
print(funnel_results)
#Conversion rate = % of leads that reach next stage
#Stage conversion = % of conversions from each stage (e.g. from lead to MQL (63.9%))


# Source Performance Analysis
source_performance <- crm_data %>%
  group_by(source) %>%
  summarise(
    total_leads = n(),
    avg_deal_value = mean(deal_value, na.rm = TRUE),
    conversion_rate = mean(stage == "Closed Won", na.rm = TRUE),
    avg_cycle_time = mean(days_in_stage, na.rm = TRUE)
  ) %>%
  mutate(
    lead_quality_score = (conversion_rate * 0.4 + (avg_deal_value/max(avg_deal_value, na.rm = TRUE)) * 0.6) * 100
  )

print("Lead Source Performance:")
print(source_performance)

# Funnel plot
(funnel_plot <- ggplot(funnel_results, aes(x = reorder(stage, -count), y = count)) +
  geom_col(aes(fill = stage), width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(count, "\n(", conversion_from_leads, "%)")), 
            vjust = 0.5, color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Lead" = "#1f77b4", "MQL" = "#ff7f0e", 
                               "SQL" = "#2ca02c", "Opportunity" = "#d62728", 
                               "Closed Won" = "#9467bd")) +
  labs(title = "Sales Funnel Performance", 
       subtitle = "Count and conversion rates by stage",
       x = "Funnel Stage", 
       y = "Number of Prospects") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)) +
  scale_y_continuous(labels = comma))

# Waterfal plot
conversion_data <- funnel_results %>%
  filter(stage != "Lead") %>%
  mutate(
    conversion_loss = 100 - stage_conversion,
    stage_label = paste0(stage, "\n", stage_conversion, "%")
  )

(conversion_plot <- ggplot(conversion_data, aes(x = stage, y = stage_conversion)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = paste0(stage_conversion, "%")), 
            vjust = -0.5, fontface = "bold") +
  labs(title = "Stage-to-Stage Conversion Rates", 
       subtitle = "Percentage converting from previous stage",
       x = "Funnel Stage", 
       y = "Conversion Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  ylim(0, 100))

# Interactive Funnel
if(require(plotly, quietly = TRUE)) {
  interactive_funnel <- plot_ly(
    data = funnel_results,
    x = ~stage,
    y = ~count,
    type = 'bar',
    text = ~paste('Stage:', stage, '<br>Count:', count, 
                  '<br>Conversion:', conversion_from_leads, '%'),
    textposition = 'inside',
    hoverinfo = 'text',
    marker = list(color = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd'))
  ) %>%
    layout(title = list(text = "Interactive Sales Funnel", x = 0.5),
           xaxis = list(title = "Funnel Stage"),
           yaxis = list(title = "Count"))
  
  print(interactive_funnel)
}

# Funnel Drop-off Analysis
dropoff_data <- funnel_results %>%
  mutate(
    lost_prospects = lag(count, default = 0) - count,
    lost_prospects = ifelse(is.na(lost_prospects) | lost_prospects < 0, 0, lost_prospects),
    retention_rate = stage_conversion
  ) %>%
  filter(stage != "Lead")

(dropoff_plot <- ggplot(dropoff_data, aes(x = stage)) +
  geom_col(aes(y = count), fill = "lightgreen", alpha = 0.7, width = 0.6) +
  geom_col(aes(y = lost_prospects), fill = "red", alpha = 0.7, width = 0.4) +
  geom_text(aes(y = count, label = count), vjust = -0.5, color = "darkgreen", fontface = "bold") +
  geom_text(aes(y = lost_prospects, label = paste("Lost:", lost_prospects)), 
            vjust = 1.5, color = "darkred", size = 3) +
  labs(title = "Funnel Drop-off Analysis", 
       subtitle = "Green = Converted, Red = Lost at each stage",
       x = "Funnel Stage", 
       y = "Number of Prospects") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))


# 2. FINANCE & REPORTING ============================================================================
# Monthly P&L Analysis with Sample Data
monthly_pnl <- data.frame(
  month = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month"),
  revenue = c(125000, 130000, 128000, 135000, 142000, 148000, 155000, 152000, 158000, 165000, 170000, 175000),
  cogs = c(37500, 39000, 38400, 40500, 42600, 44400, 46500, 45600, 47400, 49500, 51000, 52500),
  sales_marketing = c(45000, 47000, 46000, 48500, 51000, 53000, 55500, 54500, 56500, 59000, 61000, 62500),
  rd_expenses = c(25000, 26000, 25500, 27000, 28500, 29500, 31000, 30500, 31500, 33000, 34000, 35000),
  admin_expenses = c(15000, 15500, 15200, 16000, 16500, 17000, 17500, 17200, 17800, 18500, 19000, 19500)
)

# Calculate key metrics
monthly_pnl <- monthly_pnl %>%
  mutate(
    gross_profit = revenue - cogs,
    total_opex = sales_marketing + rd_expenses + admin_expenses,
    ebitda = gross_profit - total_opex,
    gross_margin = gross_profit / revenue,
    ebitda_margin = ebitda / revenue,
    revenue_growth = (revenue / lag(revenue) - 1) * 100
  )

(finance_plot1 <- ggplot(monthly_pnl, aes(x = month)) +
    geom_line(aes(y = revenue, color = "Revenue"), size = 1.2) +
    geom_line(aes(y = gross_profit, color = "Gross Profit"), size = 1.2) +
    geom_line(aes(y = ebitda, color = "EBITDA"), size = 1.2) +
    scale_color_manual(values = c("Revenue" = "#1f77b4", "Gross Profit" = "#2ca02c", "EBITDA" = "#ff7f0e")) +
    labs(title = "Financial Performance Trends", 
         subtitle = "Revenue, Gross Profit, and EBITDA over time",
         x = "Month", y = "Amount ($)", color = "Metric") +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)))


# Variance Analysis Function
variance_analysis <- function(actual, budget) {
  variance <- actual - budget
  variance_pct <- (variance / budget) * 100
  
  data.frame(
    actual = actual,
    budget = budget,
    variance = variance,
    variance_pct = variance_pct,
    status = ifelse(variance_pct > 5, "Favorable", 
                    ifelse(variance_pct < -5, "Unfavorable", "On Track"))
  )
}

(budget_variance_plot <- ggplot(budget_variance, aes(x = category, y = 1, fill = variance_pct)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = paste0(variance_pct, "%")), color = "white", fontface = "bold", size = 4) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0,
                         name = "Variance %") +
    labs(title = "Budget Variance Analysis", 
         subtitle = "Red = Over Budget, Green = Under Budget",
         x = "Category", y = "") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)))


# Sample budget vs actual analysis
budget_data <- data.frame(
  category = c("Revenue", "COGS", "Sales & Marketing", "R&D", "Admin"),
  actual = c(175000, 52500, 62500, 35000, 19500),
  budget = c(170000, 51000, 60000, 34000, 20000)
)

budget_variance <- budget_data %>%
  mutate(
    variance = actual - budget,
    variance_pct = round((variance / budget) * 100, 1),
    status = case_when(
      variance_pct > 5 ~ "Over Budget",
      variance_pct < -5 ~ "Under Budget",
      TRUE ~ "On Target"
    )
  )

print("Budget Variance Analysis (December 2024):")
print(budget_variance)

(margin_plot <- ggplot(monthly_pnl, aes(x = month)) +
  geom_line(aes(y = gross_margin * 100, color = "Gross Margin"), size = 1.2) +
  geom_line(aes(y = ebitda_margin * 100, color = "EBITDA Margin"), size = 1.2) +
  scale_color_manual(values = c("Gross Margin" = "#9467bd", "EBITDA Margin" = "#d62728")) +
  labs(title = "Profitability Margins Trend", 
       x = "Month", y = "Margin (%)", color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)))


# 3. PEOPLE & HR ANALYTICS  ============================================================================
# Sample Employee Data
set.seed(456)
employee_data <- data.frame(
  employee_id = 1:150,
  department = sample(c("Engineering", "Sales", "Marketing", "Operations", "Finance"), 150, replace = TRUE),
  hire_date = sample(seq(as.Date("2020-01-01"), as.Date("2024-12-01"), by = "day"), 150, replace = TRUE),
  salary = sample(c(65000, 75000, 85000, 95000, 105000, 125000, 145000), 150, replace = TRUE),
  performance_rating = sample(c("Exceeds", "Meets", "Below"), 150, replace = TRUE, prob = c(0.2, 0.7, 0.1)),
  status = sample(c("Active", "Terminated"), 150, replace = TRUE, prob = c(0.85, 0.15))
)

# Headcount and Turnover Analysis
hr_metrics <- function(data) {
  current_date <- as.Date("2024-12-31")
  
  metrics <- data %>%
    mutate(
      tenure_months = as.numeric(difftime(current_date, hire_date, units = "days")) / 30.44,
      hire_year = year(hire_date)
    ) %>%
    group_by(department) %>%
    summarise(
      headcount = n(),
      active_employees = sum(status == "Active"),
      avg_tenure_months = round(mean(tenure_months), 1),
      avg_salary = round(mean(salary), 0),
      turnover_rate = round((sum(status == "Terminated") / n()) * 100, 1),
      high_performers = sum(performance_rating == "Exceeds"),
      .groups = "drop"
    )
  
  return(metrics)
}

hr_summary <- hr_metrics(employee_data)
print("HR Department Metrics:")
print(hr_summary)

# Department Headcount & Metrics
(hr_plot1 <- ggplot(hr_summary, aes(x = department)) +
  geom_col(aes(y = headcount, fill = "Total Headcount"), alpha = 0.7, width = 0.6) +
  geom_col(aes(y = active_employees, fill = "Active Employees"), width = 0.4) +
  geom_text(aes(y = headcount, label = headcount), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("Total Headcount" = "lightblue", "Active Employees" = "darkblue")) +
  labs(title = "Headcount by Department", 
       x = "Department", y = "Employee Count", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5)))

(hr_plot2 <- ggplot(hr_summary, aes(x = avg_salary, y = turnover_rate)) +
    geom_point(aes(size = headcount, color = department), alpha = 0.7) +
    geom_text(aes(label = department), vjust = -1, size = 3) +
    labs(title = "Salary vs Turnover Rate by Department", 
         subtitle = "Size = Headcount",
         x = "Average Salary ($)", y = "Turnover Rate (%)", 
         color = "Department", size = "Headcount") +
    theme_minimal() +
    scale_x_continuous(labels = scales::comma) +
    theme(plot.title = element_text(hjust = 0.5)))

# Hiring Trend Analysis
hiring_trends <- employee_data %>%
  filter(status == "Active") %>%
  mutate(hire_month = floor_date(hire_date, "month")) %>%
  count(hire_month, department) %>%
  filter(hire_month >= as.Date("2024-01-01"))

print("Recent Hiring Trends (2024):")
print(head(hiring_trends, 10))


performance_data <- employee_data %>%
  count(department, performance_rating) %>%
  group_by(department) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1))

(performance_plot <- ggplot(performance_data, aes(x = department, y = percentage, fill = performance_rating)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 3) +
  scale_fill_manual(values = c("Exceeds" = "#2ca02c", "Meets" = "#1f77b4", "Below" = "#d62728")) +
  labs(title = "Performance Rating Distribution by Department", 
       x = "Department", y = "Percentage (%)", fill = "Performance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5)))



# 4. IT & SECURITY  ============================================================================
# User Access Audit Sample Data
set.seed(789)
user_access <- data.frame(
  user_id = paste0("USER", sprintf("%03d", 1:200)),
  department = sample(c("Engineering", "Sales", "Marketing", "Operations", "Finance", "HR"), 200, replace = TRUE),
  last_login = sample(seq(as.Date("2024-10-01"), as.Date("2024-12-31"), by = "day"), 200, replace = TRUE),
  active_status = sample(c("Active", "Inactive"), 200, replace = TRUE, prob = c(0.8, 0.2)),
  admin_access = sample(c(TRUE, FALSE), 200, replace = TRUE, prob = c(0.1, 0.9)),
  systems_access = sample(1:8, 200, replace = TRUE),
  failed_logins = sample(0:15, 200, replace = TRUE, prob = c(rep(0.1, 10), rep(0.05, 6)))
)

# Security Risk Assessment
security_audit <- function(data) {
  current_date <- as.Date("2024-12-31")
  
  risk_assessment <- data %>%
    mutate(
      days_since_login = as.numeric(current_date - last_login),
      risk_score = case_when(
        days_since_login > 90 & active_status == "Active" ~ "High Risk",
        failed_logins > 10 ~ "High Risk",
        admin_access == TRUE & days_since_login > 30 ~ "Medium Risk",
        systems_access > 5 & days_since_login > 60 ~ "Medium Risk",
        TRUE ~ "Low Risk"
      )
    )
  
  risk_summary <- risk_assessment %>%
    group_by(department, risk_score) %>%
    summarise(count = n(), .groups = "drop") %>%
    spread(risk_score, count, fill = 0)
  
  return(list(detailed = risk_assessment, summary = risk_summary))
}

security_results <- security_audit(user_access)
print("Security Risk Assessment by Department:")
print(security_results$summary)

# Security Risk Heatmap
security_viz_data <- security_results$detailed %>%
  count(department, risk_score) %>%
  spread(risk_score, n, fill = 0) %>%
  gather(risk_level, count, -department) %>%
  mutate(risk_level = factor(risk_level, levels = c("Low Risk", "Medium Risk", "High Risk")))

(security_plot1 <- ggplot(security_viz_data, aes(x = department, y = risk_level, fill = count)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = count), color = "white", fontface = "bold") +
  scale_fill_gradient(low = "lightgreen", high = "red", name = "User Count") +
  labs(title = "Security Risk Assessment by Department", 
       x = "Department", y = "Risk Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)))

# Inactive User Report
inactive_users <- user_access %>%
  filter(active_status == "Active" & 
           as.numeric(as.Date("2024-12-31") - last_login) > 90) %>%
  select(user_id, department, last_login, admin_access, systems_access) %>%
  arrange(last_login)

print("Users with No Login Activity (90+ days):")
print(head(inactive_users, 10))

login_activity <- user_access %>%
  mutate(
    days_since_login = as.numeric(as.Date("2024-12-31") - last_login),
    login_category = case_when(
      days_since_login <= 7 ~ "Active (≤7 days)",
      days_since_login <= 30 ~ "Recent (8-30 days)", 
      days_since_login <= 90 ~ "Inactive (31-90 days)",
      TRUE ~ "Stale (>90 days)"
    )
  ) %>%
  count(department, login_category) %>%
  mutate(login_category = factor(login_category, 
                                 levels = c("Active (≤7 days)", "Recent (8-30 days)", 
                                            "Inactive (31-90 days)", "Stale (>90 days)")))

(login_plot <- ggplot(login_activity, aes(x = department, y = n, fill = login_category)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Active (≤7 days)" = "#2ca02c", 
                               "Recent (8-30 days)" = "#1f77b4",
                               "Inactive (31-90 days)" = "#ff7f0e", 
                               "Stale (>90 days)" = "#d62728")) +
  labs(title = "User Login Activity by Department", 
       x = "Department", y = "Number of Users", fill = "Login Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5)))

# 5. PROCESS IMPROVEMENT & STRATEGIC ANALYSIS  ============================================================================
# Process Efficiency Analysis
process_data <- data.frame(
  process_name = c("Invoice Processing", "Employee Onboarding", "Lead Qualification", 
                   "Contract Approval", "Expense Reimbursement", "Bug Resolution"),
  avg_time_hours = c(24, 48, 6, 72, 12, 36),
  steps_count = c(8, 15, 5, 12, 6, 10),
  automation_level = c(0.3, 0.2, 0.7, 0.1, 0.5, 0.4),
  monthly_volume = c(450, 25, 800, 120, 300, 150),
  error_rate = c(0.05, 0.02, 0.12, 0.08, 0.03, 0.15)
)

# Process Optimization Scoring
process_optimization <- process_data %>%
  mutate(
    efficiency_score = (1 - (avg_time_hours / max(avg_time_hours))) * 100,
    automation_opportunity = (1 - automation_level) * monthly_volume * avg_time_hours,
    quality_score = (1 - error_rate) * 100,
    overall_score = (efficiency_score + quality_score) / 2,
    priority = case_when(
      automation_opportunity > 5000 ~ "High Priority",
      automation_opportunity > 2000 ~ "Medium Priority",
      TRUE ~ "Low Priority"
    )
  ) %>%
  arrange(desc(automation_opportunity))

print("Process Optimization Opportunities:")
print(process_optimization[c("process_name", "automation_opportunity", "overall_score", "priority")])

# Process Optimization Matrix
(process_plot1 <- ggplot(process_optimization, aes(x = efficiency_score, y = quality_score)) +
  geom_point(aes(size = automation_opportunity, color = priority), alpha = 0.7) +
  geom_text(aes(label = process_name), vjust = -1, size = 3, angle = 15) +
  scale_color_manual(values = c("High Priority" = "#d62728", 
                                "Medium Priority" = "#ff7f0e", 
                                "Low Priority" = "#2ca02c")) +
  labs(title = "Process Optimization Matrix", 
       subtitle = "Size = Automation Opportunity (hours/year)",
       x = "Efficiency Score", y = "Quality Score", 
       color = "Priority", size = "Automation\nOpportunity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)))

# Market Research Analysis (Strategic Project Example)
# Sample competitive analysis data
competitor_analysis <- data.frame(
  competitor = c("Company A", "Company B", "Company C", "Company D", "Our Company"),
  market_share = c(0.25, 0.20, 0.15, 0.12, 0.08),
  pricing_index = c(1.2, 1.0, 0.8, 1.1, 0.9),
  feature_score = c(8.5, 7.2, 6.8, 7.8, 8.0),
  customer_satisfaction = c(4.2, 4.5, 3.8, 4.0, 4.3),
  revenue_millions = c(120, 95, 70, 55, 35)
)

# Competitive positioning analysis
competitive_metrics <- competitor_analysis %>%
  mutate(
    value_ratio = feature_score / pricing_index,
    efficiency_ratio = revenue_millions / market_share,
    competitive_score = (feature_score * 0.3 + customer_satisfaction * 0.3 + value_ratio * 0.4)
  ) %>%
  arrange(desc(competitive_score))

print("Competitive Analysis Results:")
print(competitive_metrics)

# Automation Opportunity Ranking
(automation_plot <- ggplot(process_optimization, aes(x = reorder(process_name, automation_opportunity), 
                                                    y = automation_opportunity)) +
  geom_col(aes(fill = priority), alpha = 0.8) +
  geom_text(aes(label = paste0(round(automation_opportunity/1000, 1), "K")), 
            hjust = -0.1, fontface = "bold") +
  scale_fill_manual(values = c("High Priority" = "#d62728", 
                               "Medium Priority" = "#ff7f0e", 
                               "Low Priority" = "#2ca02c")) +
  labs(title = "Process Automation Opportunities", 
       subtitle = "Annual hours that could be automated",
       x = "Process", y = "Automation Opportunity (Hours/Year)", fill = "Priority") +
  theme_minimal() +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)))

# Competitive Analysis Radar Chart (using base R)
competitive_radar_data <- competitive_metrics %>%
  select(competitor, market_share, feature_score, customer_satisfaction, competitive_score) %>%
  mutate(
    market_share_norm = (market_share / max(market_share)) * 10,
    feature_score_norm = feature_score,
    satisfaction_norm = customer_satisfaction * 2,
    competitive_norm = (competitive_score / max(competitive_score)) * 10
  )

(comp_plot <- ggplot(competitive_metrics, aes(x = feature_score, y = customer_satisfaction)) +
    geom_point(aes(size = market_share * 100, color = competitor), alpha = 0.7) +
    geom_text(aes(label = competitor), vjust = -1.5, size = 3) +
    scale_size_continuous(name = "Market Share (%)", range = c(3, 15)) +
    labs(title = "Competitive Positioning Analysis", 
         subtitle = "Size = Market Share",
         x = "Feature Score (1-10)", y = "Customer Satisfaction (1-5)", 
         color = "Company") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(6, 9) + ylim(3.5, 4.8))

# Market Share vs Revenue Efficiency
(efficiency_plot <- ggplot(competitive_metrics, aes(x = market_share * 100, y = efficiency_ratio)) +
  geom_point(aes(color = competitor, size = revenue_millions), alpha = 0.7) +
  geom_text(aes(label = competitor), vjust = -1, size = 3) +
  labs(title = "Market Efficiency Analysis", 
       subtitle = "Revenue per Market Share Point",
       x = "Market Share (%)", y = "Revenue Efficiency (Revenue/Market Share)", 
       color = "Company", size = "Revenue ($M)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)))


# 6. REPORTING FUNCTIONS  ============================================================================
# Executive Dashboard Summary Function
create_executive_summary <- function() {
  summary_data <- list(
    revenue_growth = tail(monthly_pnl$revenue_growth, 1),
    ebitda_margin = tail(monthly_pnl$ebitda_margin, 1),
    total_pipeline = sum(crm_data$deal_value, na.rm = TRUE),
    headcount = nrow(filter(employee_data, status == "Active")),
    security_risks = nrow(filter(security_results$detailed, risk_score == "High Risk")),
    process_automation_savings = sum(process_optimization$automation_opportunity)
  )
  
  cat("=== EXECUTIVE SUMMARY ===\n")
  cat("Revenue Growth (MoM):", round(summary_data$revenue_growth, 1), "%\n")
  cat("EBITDA Margin:", round(summary_data$ebitda_margin * 100, 1), "%\n")
  cat("Sales Pipeline Value: $", format(summary_data$total_pipeline, big.mark = ","), "\n")
  cat("Active Headcount:", summary_data$headcount, "\n")
  cat("High-Risk Security Issues:", summary_data$security_risks, "\n")
  cat("Process Automation Opportunity:", round(summary_data$process_automation_savings/1000, 1), "K hours/year\n")
}

# Generate executive summary
create_executive_summary()
