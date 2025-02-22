
# 推荐通过环境变量读取
#Sys.setenv(DEEPSEEK_API_KEY = "your_api_key_here")
api_key <- Sys.getenv("DEEPSEEK_API_KEY")

library(httr)
library(jsonlite)

# API端点
url <- "https://api.deepseek.com/chat/completions"

# 请求头
headers <- add_headers(
  "Authorization" = paste("Bearer", api_key),
  "Content-Type" = "application/json"
)

# 请求体（根据API文档调整）
body <- list(
  model = "deepseek-chat",
  messages = list(
    list(role = "user", content = "Hello!")
  ),
  temperature = 0.7
)

# 发送POST请求
response <- POST(
  url = url,
  config = headers,
  body = toJSON(body, auto_unbox = TRUE),
  encode = "json"
)

# 处理响应
if (status_code(response) == 200) {
  content <- content(response, "parsed")
  cat("DeepSeek回复:", content$choices[[1]]$message$content, "\n")
} else {
  cat("错误:", status_code(response), content(response, "text"), "\n")
}

# 识别图片====
## 读取图片并转换为base64编码====
library(httr)
library(jsonlite)
library(base64enc)

img_path <- here::here("code/concept/robot.jpg") # 修改为你的图片路径
if (!file.exists(img_path)) stop("图片文件不存在")
img_data <- readBin(img_path, "raw", file.info(img_path)$size)
img_base64 <- base64enc::base64encode(img_data)

# API端点 --------------------------------------------------

chat_txt <- "
我需要对图片进行文本OCR识别，并根据下面定义的四个层级的json文件结构整理为json格式输出。
要求：（1）四个层级的json文件结构。例如第一层级为人形机器人，然后下级层级分别包括减速器、电机、传感器、控制系统、灵巧手。
然后第3层级表示下一层分类，例如‘减速器’列出了列表，其下面包括通用减速器、谐波减速器、丝杆等。
最后，最末的层级下将列出具体的上市公司名称。例如通用减速器下依次列表国贸股份、宁波东力等具体上市公司。
（2）最末的层级将定义为“stocks”，其下为具体的股票列表。
例如兆威电机将使用三个键值对，分别是：上市公司代码'id'：'003021'，上市交易所'market': 'sz'，上市公司名称'name': '兆威机电'。
你可能需要联网查询上市公司的相关信息。
（3）如果第三层级下面没有再具体识别出列表子分类，则定义其下的列表子分类为空，也即''，但该空分类下依旧需要列出识别到的上市公司列表。
（4）不需要识别百分数及前面的+号。
"

chat_txt <- gsub("\n", " ", chat_txt)

# 请求头
headers <- add_headers(
  "Authorization" = paste("Bearer", api_key),
  "Content-Type" = "application/json"
)

# 请求体（根据API文档调整）
body <- list(
  model = "deepseek-chat",
  messages = list(
    list(role = "user", content = "Hello!")
  ),
  temperature = 0.7
)

# 发送POST请求
response <- POST(
  url = url,
  config = headers,
  body = toJSON(body, auto_unbox = TRUE),
  encode = "json"
)


body <- list(
  model = "deepseek-chat",
  messages = list(
    list(
      role = "user",
      content = list(
        list(
          type = "text",
          text = chat_txt
        ),
        list(
          type = "image_url",
          image_url = list(
            url = paste0("data:image/png;base64,", img_base64)
          )
        )
      )
    )
  ),
  temperature = 1.3,
  max_tokens = 1000
)

# 发送请求 --------------------------------------------------
response <- POST(
  url,
  config = headers,
  body = toJSON(body, auto_unbox = TRUE),
  encode = "json"
)



# 处理响应 --------------------------------------------------
if (status_code(response) == 200) {
  result <- content(response, "parsed")
  cat("=== 图片分析结果 ===\n")
  cat(result$choices[[1]]$message$content)
} else {
  cat("=== 请求失败 ===\n")
  cat("状态码：", status_code(response), "\n")
  cat("错误信息：", content(response, "text"))
}
