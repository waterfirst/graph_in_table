# 필요한 패키지 설치 및 로드
install.packages(c("tidyverse", "DT", "htmlwidgets", "jsonlite"))
library(tidyverse)
library(DT)
library(htmlwidgets)
library(jsonlite)

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

# 데이터 요약
summary_data <- sales_data %>%
  group_by(product) %>%
  summarise(
    sales_trend = list(sales),
    avg_sales = mean(sales),
    total_sales = sum(sales),
    .groups = "drop"
  )

# JavaScript 함수를 문자열로 정의
sparkline_js <- 'function(data, type, row, meta) {
  if(type === "display") {
    var chartdata = JSON.parse(data);
    var width = 100;
    var height = 25;
    var padding = 2;
    
    // 최소값과 최대값 계산
    var min = Math.min.apply(null, chartdata);
    var max = Math.max.apply(null, chartdata);
    var range = max - min;
    
    // SVG 시작 태그
    var svg = "<svg width=\\"" + width + "\\" height=\\"" + height + "\\" style=\\"overflow: visible\\">";
    
    // 데이터 포인트 개수
    var n = chartdata.length;
    
    // 포인트를 연결하는 라인 생성
    var points = [];
    for(var i = 0; i < n; i++) {
      var x = padding + (i * (width - 2 * padding) / (n - 1));
      var y = height - padding - ((chartdata[i] - min) * (height - 2 * padding) / range);
      points.push(x + "," + y);
    }
    
    // 라인 추가
    svg += "<polyline fill=\\"none\\" stroke=\\"#0066cc\\" stroke-width=\\"1\\" points=\\"" + points.join(" ") + "\\"/>";
    
    // SVG 종료 태그
    svg += "</svg>";
    
    return svg;
  }
  return data;
}'

# 데이터 준비
display_data <- summary_data %>%
  mutate(
    sales_trend = sapply(sales_trend, toJSON, auto_unbox = TRUE)
  )

# DT 테이블 생성
dt_table <- datatable(
  display_data,
  options = list(
    pageLength = 25,
    columnDefs = list(
      list(
        targets = 1,  # sales_trend 열
        render = JS(sparkline_js)
      )
    )
  ),
  escape = FALSE,
  rownames = FALSE,
  colnames = c("제품명", "판매 트렌드", "평균 판매량", "총 판매량")
) %>%
  formatRound(
    columns = c("avg_sales", "total_sales"),
    digits = 0
  )

# 테이블 출력
dt_table

# HTML 파일로 저장하기 원하는 경우
saveWidget(dt_table, "sales_trends_dt.html", selfcontained = TRUE)

# 실제 데이터 사용 시 예시:
# real_data <- read.csv("your_data.csv")
# real_data$date <- as.Date(real_data$date)  # 날짜 형식으로 변환





library(sparkline)
library(formattable)
fw <- as.htmlwidget(
  formattable(
    data.frame(
      id = c("a", "b", "c"),
      sparkline = c(
        spk_chr(runif(10,0,10), type="line"),
        spk_chr(runif(10,0,10), type="bar"),
        spk_chr(runif(10,0,10), type="box")
      ),
      stringsAsFactors = FALSE
    )
  )
)
spk_add_deps(fw)


library(sparkline)
sl1 <- sparkline(
  c(5,4,5,-2,0,3),
  type='bar',
  barColor="#aaf",
  chartRangeMin=-5,
  chartRangeMax=10,
  # set an id that will make it easier to refer
  # in the next sparkline
  elementId="sparkline-for-composite"
)
sl2 <- sparkline(
  c(4,1,5,7,9,9,8,7,6,6,4,7,8,4,3,2,2,5,6,7),
  type="line",
  fillColor = FALSE,
  lineColor ='red',
  chartRangeMin = -5,
  chartRangeMax = 10
)
spk_composite(sl1, sl2)


spk_composite(
  sl1,
  values=c(4,1,5,7,9,9,8,7,6,6,4,7,8,4,3,2,2,5,6,7),
  options = list(
    type="line",
    fillColor = FALSE,
    lineColor ='red',
    chartRangeMin = -5,
    chartRangeMax = 10
  )
)
# add combination of sparkline and options as a composite
spk_composite(
  sl1,
  sl2,
  options = list(
    type="box"
  )
)



#recreate table making the row headers a column labeled Indicator Name
prevalence <- cbind("Indicator Name" = rownames(prevalence.tab), as.data.frame(prevalence.tab))
#remove the old rownames to hide
rownames(prevalence) <- c()

