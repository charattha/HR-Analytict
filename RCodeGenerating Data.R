library(dplyr)
library(lubridate)
library(DataExplorer)

set.seed(999) # เปลี่ยน seed เพื่อให้ค่าเปลี่ยน
n_rows <- 500 # เพิ่มจำนวนแถวหน่อย เพื่อให้เห็น Pattern ชัดขึ้น
current_date <- as.Date("2025-12-08")

# --- 1. Generate Basic Info ---
df <- data.frame(
    emp_id = as.character(1001:(1000 + n_rows)),
    dept = sample(c("Sales","IT","HR","CS","Logistic","Finance"), n_rows, replace = TRUE, prob = c(0.27,0.03,0.05,0.3,0.3,0.05)),
    emp_type = sample(c("Contract","Permanent"), n_rows, replace = TRUE, prob = c(0.3,0.7)),
    gender = sample(c("Male","Female"), n_rows, replace = TRUE, prob = c(0.5,0.5)),
    marital_status = sample(c("Single","Married","Divorced","Widowed"), n_rows, replace = TRUE, prob = c(0.4,0.4,0.15,0.05)),
    overtime_hours_avg = sample(0:30, n_rows, replace = TRUE), # เพิ่ม Max OT หน่อย
    training_hours = sample(0:40, n_rows, replace = TRUE)
)

# --- 2. Generate Roles ---
assign_role <- function(dept){
    if(dept == "IT") return(sample(c("IT support","Software Engineer","Data Analyst",'Data Engineer',"Tech Lead","IT Manager"),1))
    if(dept == "Sales") return(sample(c("Sales Support","Account Executive","Account Manager","Marketing Manager","Marketing Executive","Business Development Executive"),size=1,replace=TRUE))
    if(dept == "HR") return(sample(c("Recruitment Executive","HR Manager","HR Admin"),size=1,replace=TRUE))
    if(dept == "CS") return(sample(c("Customer Service Agent","Customer Service Team Lead","Customer Service Manager","Customer Service Admin"),size=1,replace=TRUE))
    if(dept == "Logistic") return(sample(c("Ops Associate", "Ops Manager", "Logistics Coordinator"),size=1,replace=TRUE))
    if(dept == "Finance") return(sample(c("Accountant Executive", "Financial Analyst", "Finance Manager") ,size=1,replace=TRUE))
}
df$role <- sapply(df$dept, assign_role)

# --- 3. Assign Job Level & Manager Status ---
manager_level <- c("Account Manager","HR Manager","IT Manager","Customer Service Manager","Ops Manager","Finance Manager","Marketing Manager")
team_lead_level <- c("Tech Lead","Customer Service Team Lead")
specialist_level <- c("Software Engineer","Data Analyst","Account Executive","Marketing Executive","Business Development Executive","Accountant Executive","Financial Analyst","Recruitment Executive","Data Engineer")

df <- df %>% 
    mutate(
        job_level = case_when(
            role %in% manager_level ~ 4,      # เปลี่ยนเป็นตัวเลข (Numeric) เพื่อคำนวณง่าย
            role %in% team_lead_level ~ 3,
            role %in% specialist_level ~ 2,
            TRUE ~ 1
        ),
        mgr_or_team_lead = if_else(job_level >= 3, 1, 0)
    )

# --- 4. Assign Salary (ผูกกับ Level ให้สมจริง) ---
df <- df %>%
    rowwise() %>%
    mutate(
        base_salary = case_when(
            job_level == 4 ~ runif(1, 80000, 150000),
            job_level == 3 ~ runif(1, 50000, 90000),
            job_level == 2 ~ runif(1, 30000, 60000),
            TRUE ~ runif(1, 15000, 35000)
        )
    ) %>% ungroup()

# สร้าง Salary Tier (ตัดตาม Quantile ของทั้งบริษัท หรือแยกตาม Level ก็ได้)
# ในที่นี้ขอตัดตามภาพรวมเพื่อสร้าง Pattern: เงินน้อยกว่าชาวบ้าน = ออกง่าย
df$salary_tier <- cut(df$base_salary, 
                      breaks = quantile(df$base_salary, probs = c(0, 0.33, 0.66, 1)), 
                      labels = c("Low", "Medium", "High"), 
                      include.lowest = TRUE)

# --- 5. Generate Distance & Age ---
df$distance_from_home_mile <- round(runif(n_rows, 1, 50))
df$age <- round(runif(n_rows, 22, 55))

# --- 6. Generate Performance & Satisfaction (ต้นเหตุ) ---
# Logic: คนทำ OT เยอะ -> Performance ดี แต่ Satisfaction ต่ำ
df <- df %>%
    mutate(
        performance_score = pmin(100, round(
            50 + (overtime_hours_avg * 1.5) + rnorm(n(), 0, 10)
        )),
        
        # Satisfaction Pattern (หัวใจสำคัญ!)
        # - เงินน้อย (Low Tier) : หักคะแนน
        # - งานหนัก (OT > 20) : หักคะแนน
        # - ไกลบ้าน (> 30) : หักคะแนน
        raw_sat = 70 + 
            (ifelse(salary_tier == "High", 10, ifelse(salary_tier == "Low", -15, 0))) - 
            (overtime_hours_avg * 0.8) - 
            (distance_from_home_mile * 0.3) +
            rnorm(n(), 0, 10), # ใส่ความสุ่มนิดหน่อย
        
        satisfaction_Score = pmax(10, pmin(100, round(raw_sat)))
    )

# --- 7. Generate Resignation Logic (พระเอกของเรา) ---
# เราจะสร้าง "Probablity to Resign" (ความน่าจะเป็นที่จะลาออก)
df <- df %>%
    mutate(
        # เริ่มต้นความเสี่ยงพื้นฐาน
        resign_prob = 0.1, 
        
        # 1. ความพอใจต่ำ -> เสี่ยงสูงมาก (+0.4)
        resign_prob = resign_prob + ifelse(satisfaction_Score < 50, 0.4, 0),
        
        # 2. Overtime หนักจัดๆ -> เสี่ยงเพิ่ม (+0.2)
        resign_prob = resign_prob + ifelse(overtime_hours_avg > 25, 0.2, 0),
        
        # 3. เป็นเด็กจบใหม่ (Level 1) -> เปลี่ยนงานบ่อย (+0.1)
        resign_prob = resign_prob + ifelse(job_level == 1, 0.1, 0),
        
        # 4. บ้านไกลเกิน -> เสี่ยงเพิ่ม (+0.1)
        resign_prob = resign_prob + ifelse(distance_from_home_mile > 40, 0.1, 0),
        
        # Cap ความน่าจะเป็นไม่ให้เกิน 0.9
        resign_prob = pmin(0.9, resign_prob)
    )

# สุ่มสถานะการลาออก โดยใช้ความน่าจะเป็นที่คำนวณมา (Coin Toss แบบถ่วงน้ำหนัก)
df$is_resigned <- rbinom(n_rows, 1, df$resign_prob)
df$emp_status <- ifelse(df$is_resigned == 1, "Resigned", "Active")

# --- 8. Fill Dates & Reasons (เก็บตกงานเอกสาร) ---
df$Tenure_Years <- round(runif(n_rows, 0.5, 10), 1)

df <- df %>%
    rowwise() %>% 
    mutate(
        resigned_Date = if_else(
            emp_status == "Resigned", 
            current_date - days(sample(1:365, 1)), 
            as.Date(NA)
        ),
        employment_Date = if_else(
            emp_status == "Active",
            current_date - days(round(Tenure_Years * 365)),
            resigned_Date - days(round(Tenure_Years * 365))
        )
    ) %>% ungroup()

# Generate Exit Reason (ต้องสอดคล้องกับ Data)
get_exit_reason <- function(status, ot, sat, salary) {
    if (status == "Active") return("N/A")
    if (sat < 40) return("Unhappy Management")
    if (ot > 20) return("Burnout")
    if (salary == "Low") return("Salary Issues")
    return("Career Change")
}

df$exit_reason <- mapply(get_exit_reason, df$emp_status, df$overtime_hours_avg, df$satisfaction_Score, df$salary_tier)

# --- Clean Up & Check ---
# ลบคอลัมน์คำนวณทิ้ง (ถ้าไม่อยากให้เห็นเฉลย)
final_df <- df %>% select(-raw_sat, -resign_prob, -is_resigned) 

# แปลง is_resigned กลับมาใหม่จาก emp_status เพื่อใช้เช็ก
final_df$target <- ifelse(final_df$emp_status == "Resigned", 1, 0)

# Check Correlation
print("--- Correlation Check ---")
cor(final_df %>% select(target, satisfaction_Score, overtime_hours_avg, distance_from_home_mile, job_level))

# บันทึกไฟล์
write.csv(final_df, "Generate_HR_Dataset_Patterned.csv", row.names = FALSE)
print("File Generated Successfully!")