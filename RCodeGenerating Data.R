library(dplyr)
library(lubridate)
library(DataExplorer)

set.seed(61)
n_rows <- 300
current_date <- as.Date("2025-12-08")

df <- data.frame(
    emp_id = as.character(1001:(1000 + n_rows)) ,
    dept = sample( c("Sales","IT","HR","CS","Logistic","Finance"),n_rows  ,replace = TRUE,prob = c(0.27,0.03,0.05,0.3,0.3,0.05)),
    emp_type = sample(c("Contract","Permanant"),n_rows,replace = TRUE,prob=c(0.3,0.7)),
    gender = sample(c("Male","Female"),n_rows,replace = TRUE,prob=c(0.5,0.5)) ,
    marital_status = sample(c("Single","Married","Divorce","Widowed"),n_rows,replace=TRUE,prob=c(0.6,0.37,0.1,0.03)),
    overtime_hours_avg = sample(0:25, n_rows, replace = TRUE),
    training_hours = sample(0:40, n_rows, replace = TRUE)
    
)

## generate position

assign_role <- function(dept){
    if(dept == "IT") return(sample(c("IT support","Softwere Engineer","Data Analyst",'Data Engineer',"Tech Lead","IT Manager"),1))
    if(dept == "Sales") return(sample(c("Sales Support","Account Executive","Account Manager","Marketing Manager","Marketing Executive","Business Development Executive"),size=1,replace=TRUE,prob=c(0.1,0.3,0.1,0.1,0.3,0.1)))
    if(dept == "HR") return(sample(c("Recruitment Exective","HR Manager","HR Admin"),size=1,replace=TRUE,prob=c(0.4,0.2,0.4)))
    if(dept == "CS") return(sample(c("Customer Service Agent","Customer Service Team Lead","Customer Service Manager","Customer Service Admin"),size=1,replace=TRUE,prob=c(0.5,0.2,0.1,0.2)))
    if(dept == "Logistic") return(sample(c("Ops Associate", "Ops Manager", "Logistics Coordinator"),size=1,replace=TRUE,prob=c(0.5,0.2,0.3)))
    if (dept == "Finance") return(sample(c("Accountant Executive", "Financial Analyst", "Finance Manager") ,size=1,replace=TRUE,prob=c(0.5,0.3,0.2)))
}


df$role <- sapply(df$dept,assign_role)

## assigned job level

manager_level <- c("Account Manager","HR Manager","IT Manager","Customer Service Manager","Ops Manager","Finance Manager","Marketing Manager")

team_lead_level <- c("Tech Lead","Customer Service Team Lead")

specialist_level <- c("Softwere Engineer","Data Analyst","Account Executive","Marketing Executive","Business Development Executive","Accountant Executive","Financial Analyst","Recruitment Exective","Data Engineer")

general_level <- c("HR Admin", "IT support", "Sales Support", 
                   "Customer Service Admin", "Customer Service Agent", "Ops Associate","Logistics Coordinator")

df <- df %>% 
    mutate(
        job_level = case_when(
            role %in% manager_level ~ "1",
            role %in% team_lead_level ~ "2",
            role %in% specialist_level ~ "2",
            TRUE ~ "3"
        )
    )

df <- df %>% 
    mutate(
        mgr_or_team_lead = case_when(
            role %in% manager_level ~ "1",
            role %in% team_lead_level ~ "1",
            TRUE ~ "0"
        )
    )

## assigned annual salary

all_roles <- c(manager_level, team_lead_level, specialist_level, general_level)

prob_weights <- c(rep(0.05, length(manager_level)),
                  rep(0.10, length(team_lead_level)),
                  rep(0.35, length(specialist_level)),
                  rep(0.50, length(general_level)))

job_role <- sample(all_roles, n_rows, replace = TRUE, prob = prob_weights)

df$base_salary <- 0 

df$base_salary[df$role %in% manager_level] <- runif(sum(df$role %in% manager_level),min = 80000, max = 150000)

df$base_salary[df$role %in% team_lead_level] <- runif(sum(df$role %in% team_lead_level),min = 30000, max = 75000)

df$base_salary[df$role %in% specialist_level] <- runif(sum(df$role %in% specialist_level),min = 30000, max = 75000)

is_general <- df$role %in% general_level
df$base_salary[is_general] <- round(runif(sum(is_general), min = 15000, max = 30000), 0)

## assigned salary tier
df$salary_tier <- ave(df$base_salary, df$job_level, FUN = function(salary) {
    cut(salary, 
        breaks = 3, 
        labels = c("Low", "Medium", "High"),
        include.lowest = TRUE)
})

## generate distance runif จะใช่ง่ายกว่า sample กรณีสุ่มต้วเลข
n_low  <- round(n_rows * 0.6)
n_medium  <- round(n_rows * 0.3)
n_high  <- n_rows - n_low - n_medium

part1 <- runif(n = n_low, min = 0.5,max=20)
part2 <- runif(n = n_medium, min = 20,max=30)
part3 <- runif(n = n_high, min = 30,max=50)

df$distance_from_home_mile <- round(sample(c(part1,part2,part3)),0)

## generate age
range_low  <- seq(22,30,by=1)
range_medium  <- seq(31,50,by=1)
range_high  <- seq(50,55,by=1)

age_low <- n_rows * 0.4
age_medium <- n_rows * 0.4
age_high <- n_rows - age_medium - age_low

age1 <- sample(range_low,age_low,replace = TRUE)
age2 <- sample(range_medium,age_medium,replace = TRUE)
age3 <- sample(range_high,age_high,replace = TRUE)

df$age <- sample(c(age1,age2,age3))

## emp_status

df$emp_status <- ifelse(df$emp_type == "Permanant ",
                        sample(c("Active","Resigned"),n_rows,replace=TRUE,prob = c(0.7,0.3)),
                        sample(c("Active","Resigned"),n_rows,replace=TRUE,prob=c(0.6,0.4)))

## termination type
df$termination_type <- ifelse(df$emp_status == "Resigned",
                        sample(c("No_show","Follow SOP","Short Notice"),n_rows,replace=TRUE,prob = c(0.1,0.6,0.3)),
"N/A")

## generate active date and resigned date

df$Tenure_Years <- round(rgamma(n_rows, shape = 2, scale = 2), 1)

df <- df %>%
    # ใช้ rowwise() เพื่อให้ฟังก์ชันสุ่มทำงานแยกกันทีละบรรทัด (แต่ละคนได้วันไม่ซ้ำกัน)
    rowwise() %>% 
    mutate(
        # 1. สร้างวันที่ลาออก (สุ่มย้อนหลังไม่เกิน 2 ปี หรือ 730 วัน สำหรับคน Resigned)
        resigned_Date = if_else(
            emp_status == "Resigned", 
            current_date - days(sample(1:730, 1)), 
            as.Date(NA)
        ),
        
        # 2. กำหนดจุดอ้างอิง: ถ้ายังอยู่ใช้วันนี้ ถ้าออกแล้วใช้วันลาออก
        reference_Date = if_else(
            emp_status == "Active", 
            current_date, 
            resigned_Date
        ),
        
        # 3. คำนวณวันเริ่มงาน: เอาจุดอ้างอิง - อายุงาน
        employment_Date = reference_Date - days(round(Tenure_Years * 365)) 
    ) %>%
    ungroup() %>% # ยกเลิก rowwise เพื่อคืนรูปเป็น data frame ปกติ
    select(-reference_Date)

## performance_score

n_low  <- round(n_rows * 0.2)
n_medium  <- round(n_rows * 0.6)
n_high  <- n_rows - n_low - n_medium

part1 <- runif(n = n_low, min = 20,max=40)
part2 <- runif(n = n_medium, min = 40,max=80)
part3 <- runif(n = n_high, min = 80,max=100)

df$performance_score <- round(sample(c(part1,part2,part3)))

## satisfaction_score

df$satisfaction_Score <- 50 + 
    (df$performance_score * 0.3) + # คนเก่งมีความสุขเพิ่ม (Max +30)
    (df$salary_tier * 5) - 
    (df$distance_from_home_mile * 0.5) - # บ้านไกลลดความสุข (Max -25)
    (df$overtime_hours_avg * 1.0) +    # ทำ OT เยอะลดความสุข (Max -20)
    rnorm(n_rows, mean = 0, sd = 5)    # ใส่ความมั่วเข้าไป +/- 5 คะแนน

df$satisfaction_Score <- ifelse(df$emp_status == "Resigned", 
                                df$satisfaction_Score - 15, 
                                df$satisfaction_Score)

# ตบตัวเลขให้อยู่ในกรอบ 10 - 100 และปัดเศษ
df$satisfaction_Score <- round(pmax(10, pmin(100, df$satisfaction_Score)))

## Reason for leaving
get_exit_reason <- function(emp_status, overtime_hours_avg, salary_tier, training_hours) {
    if (emp_status == "Active") return("N/A")
    if (overtime_hours_avg > 15) return("Burnout/Workload")
    if (salary_tier == "Low") return("Better Compensation")
    if (training_hours < 5) return("Career Growth")
    return(sample(c("Management Issues", "Personal/Relocation"), 1))
}

df <- df %>%
    mutate(
        # สร้างเฉพาะคนที่ลาออก (Active เป็น NA)
        exit_sentiment = case_when(
            emp_status == "Active" ~ "N/A",
            
            # ถ้าคะแนนความสุขต่ำกว่า 40 -> มักจะด่าบริษัท (Negative)
            satisfaction_Score < 40 ~ "Negative",
            
            # ถ้าคะแนน 40-70 -> เฉยๆ หรือบ่นบ้างชมบ้าง (Neutral)
            satisfaction_Score >= 40 & satisfaction_Score <= 70 ~ "Neutral",
            
            # ถ้าคะแนนสูงกว่า 70 -> จากกันด้วยดี (Positive)
            satisfaction_Score > 70 ~ "Positive"
        )
    )

df$exit_reason <- mapply(get_exit_reason,df$emp_status,df$overtime_hours_avg,df$salary_tier,df$training_hours)

write.csv(df, "Generate_HR_Dataset.csv", row.names = FALSE)

# Preview the data
## print(paste("File generated with", nrow(df), "rows."))
head(df)

##create_report(df)

## use_git_config(user.name = "charattha", user.email = "email@example.com")