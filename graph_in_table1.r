# 필요한 패키지 설치 및 로드
install.packages(c("tidyverse", "gridExtra", "gt"))
library(tidyverse)
library(gridExtra)
library(gt)

# 샘플 데이터 생성
set.seed(123)
dates <- seq.Date(from = as.Date("2023-01-01"), 
                  by = "month", 
                  length.out = 12)

# 데이터프레임 생성
sales_data <- expand.grid(
  date = dates,
  product = paste("제품", 1:5)
) %>%
  arrange(product, date) %>%
  group_by(product) %>%
  mutate(
    sales = round(runif(n(), 100, 1000))
  ) %>%
  ungroup()

# 미니 차트 생성 함수
create_mini_plot <- function(data) {
  p <- ggplot(data, aes(x = date, y = sales)) +
    geom_line(color = "blue") +
    theme_void() +  # 축과 배경 제거
    theme(
      plot.margin = unit(c(0,0,0,0), "cm"),
      panel.background = element_rect(fill = "white")
    )
  
  # 그래프를 이미지 파일로 저장
  filename <- tempfile(fileext = ".png")
  ggsave(filename, p, width = 2, height = 1, dpi = 100)
  return(filename)
}

# 각 제품별 요약 데이터 생성
summary_data <- sales_data %>%
  group_by(product) %>%
  summarise(
    avg_sales = mean(sales),
    total_sales = sum(sales),
    .groups = "drop"
  )

# 각 제품별로 미니 차트 생성
plot_files <- sales_data %>%
  group_split(product) %>%
  map(create_mini_plot)

# 미니 차트 파일 경로를 데이터프레임에 추가
summary_data$trend_plot <- unlist(plot_files)

# GT 테이블 생성
final_table <- summary_data %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = trend_plot),
    fn = function(x) {
      map_chr(x, function(path) {
        local_image(
          filename = path,
          height = 50
        )
      })
    }
  ) %>%
  cols_label(
    product = "제품명",
    avg_sales = "평균 판매량",
    total_sales = "총 판매량",
    trend_plot = "판매 트렌드"
  ) %>%
  fmt_number(
    columns = c(avg_sales, total_sales),
    decimals = 0
  ) %>%
  tab_header(
    title = "제품별 판매 현황",
    subtitle = "2023년 월별 판매 트렌드"
  )

final_table

# 테이블 출력 및 저장
gtsave(final_table, "sales_trends_with_plots.html")

# 임시 파일 삭제
unlink(plot_files)
