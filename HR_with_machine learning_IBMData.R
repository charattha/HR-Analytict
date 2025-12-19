library(dplyr)
library(caret)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(showtext)
library(gt)

font_add_google("Kanit", "kanit")
showtext_auto()


#import data
df <- read.csv("HR-Employee-Attrition.csv")

#check null value
colSums(is.na(df))

#clean data ## จะลบคอลั้มเฉลยออกไป
df_clean <- df %>% 
    mutate(Attrition_Numeric = ifelse(Attrition == "Yes",1,0)) %>% 
    select(
        -Attrition,
        EmployeeNumber
    )

## แปลเป็น factor เพื่อให้ model นั้นสามารถทำงานได้
df_clean$Attrition_Numeric <- as.factor(df_clean$Attrition_Numeric)

# เผื่อตัดข้อมูลไม่สำคัญออกไป
zerovar_cols <- nearZeroVar(df_clean)

if(length(zerovar_cols) > 0) {
    df_clean <- df_clean[, -zerovar_cols]
}

## split data
set.seed(49)
n <- nrow(df_clean)

## ใช้แทนการ split data ทั่วไป เพื่อรักษาสัดส่วนให้สุ่มได้กลุ่มคนลาออกอย่างเหมาะสม
train_id <- createDataPartition(df_clean$Attrition_Numeric, p = 0.7, list = FALSE)

## จะทำการแยก data เป็น 2 ก้อนอันนึงเป็นtrain และ test
train_df <- df_clean[train_id,]
test_df <- df_clean[-train_id,]

# กำหนดวิธีการทดสอบแบบ Cross-Validation 5 รอบ
train_ctrl <- trainControl(method ="cv",
                           number = 5)

# เริ่มสร้างโมเดล Random Forest
model <- train(Attrition_Numeric ~ .,
               data = train_df,
               method = "rf",
               trControl = train_ctrl,
               metric = "Accuracy",
               nodesize = 11,
               ntree = 110)

# สั่งทำนายผล (เก็บลงตัวแปร)
pred_train <- predict(model,newdata = train_df)
p_attrion <- predict(model,newdata = test_df)

# --- ตรวจสอบผลลัพธ์ (Model Evaluation) ---

# 1. ตรวจสอบกับข้อมูลชุด Train (ดูว่าโมเดลเรียนรู้ได้ดีแค่ไหน)
confusionMatrix(pred_train, train_df$Attrition_Numeric,dnn = c("Prediction", "Actual"))

# 2. ตรวจสอบกับข้อมูลชุด Test (ดูว่าเจอกับข้อมูลจริงแล้วแม่นไหม)
cm_test <- confusionMatrix(p_attrion, test_df$Attrition_Numeric,dnn = c("Prediction", "Actual"))

cm_data <- as.data.frame(cm_test$table)

cm_data <- cm_data %>%
    mutate(Type = case_when(
        Prediction == "1" & Actual == "1" ~ "TP: ทายถูก (คนจะลาออก)",
        Prediction == "0" & Actual == "0" ~ "TN: ทายถูก (คนอยู่ต่อ)",
        Prediction == "1" & Actual == "0" ~ "FP: สัญญาณลวง (False Alarm)",
        Prediction == "0" & Actual == "1" ~ "FN: อันตราย! (หาคนลาออกไม่เจอ)"
    ))

matrix_plot <- ggplot(cm_data, aes(x = Actual, y = Prediction, fill = Type)) +
    geom_tile(color = "white", size = 1) + 
    geom_text(aes(label = Freq), size = 8, color = "white", fontface = "bold") +
    
    # --- ส่วนสำคัญ: กำหนดสีให้สื่อความหมาย ---
    scale_fill_manual(values = c(
        "TP: ทายถูก (คนจะลาออก)" = "#28A745",
        "TN: ทายถูก (คนอยู่ต่อ)" = "#28A745",
        "FP: สัญญาณลวง (False Alarm)" = "#DC3545", 
        "FN: อันตราย! (หาคนลาออกไม่เจอ)" = "#DC3545"
    )) +
    
    labs(title = "ผลการทำนายแบบแยกประเภท (Confusion Matrix)",
         subtitle = "Green/Blue = ดี (ทายถูก), Red = อันตราย (ทายผิด)",
         x = "Actual (ความเป็นจริง)",
         y = "Prediction (โมเดลทำนาย)") +
    theme_minimal() +
    theme(text = element_text(family = "kanit"), 
          plot.title = element_text(size = 22, face = "bold"), # ปรับหัวข้อใหญ่
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 14),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_blank()) # ซ่อนหัวข้อ Legend

importance <- varImp(model, scale = FALSE)

importance_df <- importance$importance %>% 
    rownames_to_column(var = "Feature") %>% 
    rename(Score = Overall) %>% 
    arrange(desc(Score))

# คัดมาเฉพาะ 20 อันดับแรก (จะได้ไม่รก)
plot_data <- importance_df %>% 
    head(20)

# สร้างกราฟ Lollipop
ggplot(plot_data, aes(x = Score, y = reorder(Feature, Score))) + 
    # เส้นก้านลูกอม
    geom_segment(aes(x = 0, xend = Score, y = Feature, yend = Feature), 
                 color = "grey50") +
    # หัวลูกอม
    geom_point(size = 4, color = "#FF6B6B") +
    # ใส่ตัวเลขกำกับที่หัวลูกอม (จะได้ดูเป็นตาราง)
    geom_text(aes(label = round(Score, 2)), 
              hjust = -0.5, size = 3.5, color = "#333333") +
    
    # จัดธีมให้สะอาด
    theme_minimal() +
    labs(title = "Feature Importance: ปัจจัยที่มีผลต่อการลาออก",
         subtitle = "เรียงลำดับจากมากไปน้อย",
         x = "คะแนนความสำคัญ (Importance Score)",
         y = "") + # ซ่อนชื่อแกน Y เพราะชื่อตัวแปรมันบอกอยู่แล้ว
    theme(
        panel.grid.major.y = element_blank(), # ลบเส้นตารางแนวนอนทิ้ง
        text = element_text(family = "kanit") # ใช้ฟอนต์สวยๆ
    ) +
    xlim(0, max(plot_data$Score) + 2) # เผื่อที่ขวาสุดไว้ให้ตัวเลขหน่อย

## นำตัวแปรที่ได้หา ค่าความเสี่ยงให้เป็น รูปแบบของ score 
pred_prob <- predict(model, newdata = test_df, type = "prob")
risk_scores <- pred_prob[, "1"] * 100


## นำ risk_score ขอแต่ละคนมาเป็น benchmark ในการหาค่าเฉลี่ยออกมา
benchmark_df <- data.frame(
    Actual = test_df$Attrition_Numeric,
    Score = risk_scores
)

resigned_stats <- benchmark_df %>%
    filter(Actual == 1) %>%
    summarise(
        Min_Score = min(Score),      # คะแนนต่ำสุดที่ลาออก
        Avg_Score = mean(Score),     # คะแนนเฉลี่ยคนลาออก
        Median_Score = median(Score),# ค่ากลาง
        Q25 = quantile(Score, 0.25)  # 25% ของคนลาออก คะแนนจะเริ่มที่ตรงนี้
    )

outliner_graph <- ggplot(benchmark_df, aes(x = as.factor(Actual), y = Score, fill = as.factor(Actual))) +
    
    # ปรับแต่งตัวกล่อง (Outlier สีแดง จุดใหญ่หน่อยจะได้เห็นชัดๆ)
    geom_boxplot(outlier.colour = "red", outlier.size = 2, alpha = 0.8) +
    
    # กำหนดสี (ใช้สีที่ดู Modern ขึ้นนิดนึง)
    scale_fill_manual(values = c("#A6CEE3", "#FB9A99")) +
    
    # เปลี่ยนป้ายแกน X ให้เป็นคำพูดคน (ไม่ต้องไปแก้ data จริง)
    scale_x_discrete(labels = c("0" = "Active", 
                                "1" = "Resigned")) +
    
    labs(title = "การกระจายตัวของ Risk Score",
         subtitle = "เปรียบเทียบระหว่างกลุ่มที่อยู่ต่อ vs ลาออก",
         x = NULL, 
         y = "โอกาสลาออก (%)") +
    
    theme_minimal() +
    
    # ลบ Legend ด้านขวาออก (เกะกะ)
    theme(text = element_text(family = "kanit", size = 14),
          legend.position = "none",
          plot.title = element_text(face = "bold", size = 14))

final_report <- test_df %>%
    mutate(Risk_Score = risk_scores) %>%
    mutate(Risk_Level = case_when(
        Risk_Score >= 75 ~ "Critical",
        Risk_Score >= 60 ~ "High",
        Risk_Score >= 50 ~ "Medium",
        TRUE ~ "Normal"
    )) %>%
    select(EmployeeNumber,Attrition_Numeric,Department, Risk_Score, Risk_Level) %>%
    arrange(desc(Risk_Score),Attrition_Numeric)

summary_table <- final_report %>%
    # นับจำนวนคนแยกตามกลุ่มเสี่ยงและสถานะ (0/1)
    group_by(Risk_Level, Attrition_Numeric) %>%
    summarise(Count = n(), .groups = "drop") %>%
    
    # แปลงแถวเป็นคอลัมน์ (Active, Resigned)
    pivot_wider(names_from = Attrition_Numeric, 
                values_from = Count, 
                values_fill = 0) %>%
    rename(Active = `0`, Resigned = `1`) %>%
    
    # คำนวณยอดรวม และ % การลาออก (Conversion Rate)
    mutate(
        Total = Active + Resigned,
        Resign_Rate = (Resigned / Total) * 100
    ) %>%
    
    # จัดเรียงลำดับความเสี่ยง (เอา Critical ไว้บนสุด)
    mutate(Risk_Level = factor(Risk_Level, levels = c("Critical", "High", "Medium", "Normal"))) %>%
    arrange(Risk_Level)

# 2. แสดงผลเป็นตารางสวยงาม
summary_table %>%
    gt() %>%
    opt_table_font(
        font = list(
            google_font("Kanit"), # ดึง Kanit จาก Google
            "Thonburi",           # สำรองเผื่อใช้ Mac
            default_fonts()       # สำรองสุดท้าย
        )
    ) %>% 
    # ใส่หัวข้อ
    tab_header(
        title = md("**สรุปผลการจำแนกกลุ่มเสี่ยง (Risk Segmentation Summary)**"),
        subtitle = "เทียบจำนวนพนักงานที่อยู่ต่อ (Active) vs ลาออก (Resigned) ในแต่ละระดับความเสี่ยง"
    ) %>%
    
    # จัดรูปแบบตัวเลข (ให้มีทศนิยม 2 ตำแหน่ง สำหรับ %)
    fmt_number(
        columns = c(Resign_Rate),
        decimals = 2,
        pattern = "{x}%" # เติมเครื่องหมาย % ต่อท้าย
    ) %>%
    
    # เปลี่ยนชื่อหัวคอลัมน์ให้สื่อความหมาย
    cols_label(
        Risk_Level = "ระดับความเสี่ยง",
        Active = "ยังอยู่ (คน)",
        Resigned = "ลาออก (คน)",
        Total = "รวมทั้งหมด (คน)",
        Resign_Rate = "อัตราการลาออกจริง"
    ) %>%
    
    # ไฮไลท์สีในช่อง Resign Rate ให้ดูง่าย (ยิ่งเยอะยิ่งแดง)
    data_color(
        columns = Resign_Rate,
        colors = scales::col_numeric(
            palette = c("#E8F5E9", "#FFEBEE", "#FFCDD2", "#EF5350"), # ไล่สีเขียวอ่อน -> แดง
            domain = NULL
        )
    )

