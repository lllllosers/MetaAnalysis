# ================== Meta分析：高精度中国地图(十段线+南海小图) ==================
# 依赖：readxl writexl dplyr stringr tidygeocoder ggplot2 sf cowplot ggmapcn
# install.packages(c("readxl","writexl","dplyr","stringr","tidygeocoder",
#                    "ggplot2","sf","cowplot","ggmapcn"))

library(readxl) 
library(writexl) 
library(dplyr) 
library(stringr)
library(tidygeocoder)
library(ggplot2)
library(sf)
library(cowplot)
library(ggmapcn)


# -------- 1) 基本参数 --------
xlsx_in  <- "./有机肥替代/Meta筛选记录模板_有机肥替代.xlsx"
sheet_in <- "最终版（暂定作图）"          # 这里改成你需要的工作表
col_place <- "研究地点"          # 地点列名自动检测，不用手动改
col_crop  <- NULL               # 可选：分类上色列（如“作物类型”），没有就保持 NULL

xlsx_out <- "Geocoded_with_checklist.xlsx"
png_out  <- "sites_map_CN_ggmapcn.png"
tiff_out <- "sites_map_CN_ggmapcn.tiff"
jpg_out  <- "sites_map_CN_ggmapcn.jpg"

# -------- 2) 读取数据 --------
print(readxl::excel_sheets(xlsx_in))
df <- readxl::read_excel(xlsx_in, sheet = sheet_in)

# -------- 3) 自动识别“研究地点”和经纬度列 --------
names_norm <- trimws(names(df))

# 研究地点列候选
cand_place <- c("研究地点(省/国家)", "研究地点（省/国家）", "研究地点/国家",
                "研究地点", "Study region/country", "研究区域", "研究地点（省／国家）")
hit <- cand_place[cand_place %in% names_norm][1]
if (is.na(hit)) stop("未找到‘研究地点’列，请检查表头")
col_place <- hit

# 经纬度列候选
cand_lon <- c("lon","longitude","LONGITUDE","Long","LONG",
              "经度","东经","经度(°)","经度（°）","E")
cand_lat <- c("lat","latitude","LATITUDE","Lat","LAT",
              "纬度","北纬","纬度(°)","纬度（°）","N")

lon_src <- cand_lon[cand_lon %in% names_norm][1]
lat_src <- cand_lat[cand_lat %in% names_norm][1]

# ---- 更鲁棒：从字符串中直接提取数字解析，经/纬方向靠 W/S 或负号 ----
dms_to_dd <- function(x, is_lon = TRUE) {
  x <- as.character(x)
  
  parse_one <- function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(NA_real_)
    
    # 规范化：大写、去空格、全角逗号/点改成半角
    s0 <- toupper(s)
    s0 <- gsub("\\s+", "", s0)
    s0 <- gsub("，|．", ".", s0)
    
    # 方向/符号：有 W 或 S 视为负；字符串带负号也视为负
    sign_dir <- ifelse(grepl("[WS]", s0), -1, 1)
    sign_num <- ifelse(grepl("^-", s0), -1, 1)
    sg <- ifelse(sign_dir == -1 || sign_num == -1, -1, 1)
    
    # 提取所有数字（含小数）；依次视为 度/分/秒
    nums <- stringr::str_extract_all(s0, "-?\\d+(?:\\.\\d+)?")[[1]]
    if (length(nums) == 0) return(NA_real_)
    vals <- suppressWarnings(as.numeric(nums))
    
    deg <- abs(vals[1])
    mn  <- if (length(vals) >= 2) vals[2] else 0
    sec <- if (length(vals) >= 3) vals[3] else 0
    
    val <- sg * (deg + mn/60 + sec/3600)
    
    # 合理性检查
    if (!is.finite(val)) return(NA_real_)
    if (is_lon && (val < -180 || val > 180)) return(NA_real_)
    if (!is_lon && (val < -90  || val > 90 )) return(NA_real_)
    val
  }
  
  vapply(x, parse_one, numeric(1))
}

# ---- 转换并写入 lon/lat ----
if (!is.na(lon_src)) df$lon <- dms_to_dd(df[[lon_src]], is_lon = TRUE)
if (!is.na(lat_src)) df$lat <- dms_to_dd(df[[lat_src]], is_lon = FALSE)

# 如果还没有 lon/lat，补充空列（后续靠地理编码）
if (!("lon" %in% names(df))) df$lon <- NA_real_
if (!("lat" %in% names(df))) df$lat <- NA_real_

cat(">>> 检查：源列名 lon_src =", lon_src, "; lat_src =", lat_src, "\n")

cat(">>> 转换后非NA计数：",
    "lon =", sum(!is.na(df$lon)), "; lat =", sum(!is.na(df$lat)), "\n")

# 看看前几行原始值与转换结果（若源列存在）
if (!is.na(lon_src)) {
  cat(">>> 样例（原lon列 -> lon）：\n")
  print(head(data.frame(raw=df[[lon_src]], lon=df$lon), 10))
}
if (!is.na(lat_src)) {
  cat(">>> 样例（原lat列 -> lat）：\n")
  print(head(data.frame(raw=df[[lat_src]], lat=df$lat), 10))
}

# 找出失败的样本（原值非空但 lon/lat 是 NA）
bad_idx <- which(!is.na(df[[lat_src]]) | !is.na(df[[lon_src]]))
bad_idx <- bad_idx[is.na(df$lon) | is.na(df$lat)]
if (length(bad_idx)>0) {
  cat(">>> 解析失败样本（最多10行）：\n")
  print(head(data.frame(
    place = df[[col_place]][bad_idx],
    lon_raw = if (!is.na(lon_src)) df[[lon_src]][bad_idx] else NA,
    lat_raw = if (!is.na(lat_src)) df[[lat_src]][bad_idx] else NA,
    lon = df$lon[bad_idx],
    lat = df$lat[bad_idx]
  ), 10))
}

# -------- 4) 清洗地名并列出待编码清单 --------
df <- df %>%
  mutate(row_id = row_number(),
         !!col_place := str_squish(.data[[col_place]]))

todo <- df %>%
  filter(is.na(lon) | is.na(lat)) %>%
  select(row_id, !!sym(col_place)) %>%
  rename(address = !!sym(col_place)) %>%
  filter(!is.na(address), address != "")

# -------- 5) 不做地理编码，直接使用表内 lon/lat --------
df2 <- df

# （可选）简单提示：统计缺失/越界行数，方便你在 Need_manual_check 里核对
n_missing <- sum(is.na(df2$lon) | is.na(df2$lat) |
                   df2$lon < -180 | df2$lon > 180 |
                   df2$lat <  -90 | df2$lat >  90)
if (n_missing > 0) message("提示：有 ", n_missing, " 行经纬度缺失或越界，将写入 Need_manual_check。")


# -------- 6) 导出（总表 + 待校对） --------
need_check <- df2 %>%
  filter(is.na(lon) | is.na(lat)) %>%
  select(row_id, !!sym(col_place), lon, lat)

write_xlsx(list(Final_data = df2,
                Need_manual_check = need_check),
           xlsx_out)
message("已导出：", xlsx_out)

# -------- 7) 绘制分布图（ggmapcn：含十段线 + 南海小图） --------
sites <- df2 %>% filter(!is.na(lon), !is.na(lat))
if (nrow(sites) == 0) stop("没有可用坐标点，请先在 Need_manual_check 中补充后重跑。")

# 投影（中国常用）：等距方位投影；适度外扩避免裁切
china_proj <- "+proj=aeqd +lat_0=35 +lon_0=105 +ellps=WGS84 +units=m +no_defs"
xlim_main <- c(78.5, 135.5)   # 比原来多留 0.5° 边距
ylim_main <- c(13.5, 55.5)

# 主图（全国范围，含省界 + 十段线；点位叠加）
p_main <- ggplot() +
  geom_mapcn(
    data = NULL,
    crs = china_proj,
    admin_level = "province",
    fill = "#BEEF8F", 
    color = "NA", 
    linewidth = 0.15
  ) +  # 半透明底色
  geom_boundary_cn(
    crs = china_proj,
    province_color = "black", province_size = 0.25,
    ten_segment_line_color = "black", ten_segment_line_size = 0.4
  ) +
  ggspatial::annotation_scale(
    location = "bl", width_hint = 0.1,
    height = unit(0.1, "cm"), text_cex = 0.7, line_width = 0.15,
  )+
  {
    if (!is.null(col_crop) && col_crop %in% names(sites)) {
      geom_loc(
        data = sites, lon = "lon", lat = "lat", crs = china_proj,
        mapping = aes(fill = .data[[col_crop]]),
        shape = 21, size = 2.0, color = "black", alpha = 0.95, linewidth = 0.2
      )
    } else {
      geom_loc(
        data = sites, lon = "lon", lat = "lat", crs = china_proj,
        shape = 21, size = 1.2, fill = "red", color = "black",
        alpha = 0.95, linewidth = 0.2
      )
    }
  } +
  coord_proj(crs = china_proj, xlim = xlim_main, ylim = ylim_main, expand = FALSE) +
  # ——经纬度刻度位置（网格线就按这些刻度画）——
  scale_x_continuous(breaks = c(80, 90, 100, 110, 120, 130)) +
  scale_y_continuous(breaks = c(20, 30, 40, 50)) +
  theme_minimal(base_size = 13) +
  theme(
    # 主要增改：开启主网格线（浅灰虚线），关闭次网格线
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(14, 18, 14, 18),
  ) +
  labs(x = NULL, y = NULL)   # 去掉标题与副标题

# 南海小图（范围可按需微调）
scs_xlim <- c(108, 122)
scs_ylim <- c(2, 23)
sites_scs <- sites %>%
  dplyr::filter(dplyr::between(lon, scs_xlim[1], scs_xlim[2]),
                dplyr::between(lat, scs_ylim[1], scs_ylim[2]))

p_scs <- ggplot() +
  geom_mapcn(
    data = NULL,
    crs = china_proj,
    admin_level = "province",
    fill = "#BEEF8F", 
    color = "NA", 
    linewidth = 0.15
  ) +  # 半透明底色
  geom_boundary_cn(
    crs = china_proj,
    province_color = "black", province_size = 0.25,
    ten_segment_line_color = "black", ten_segment_line_size = 0.35
  ) +
  ggspatial::annotation_scale(
    location = "br", width_hint = 0.3,
    height = unit(0.08, "cm"), text_cex = 0.35, line_width = 0.15,
  )+

  {
    if (nrow(sites_scs) > 0) {
      if (!is.null(col_crop) && col_crop %in% names(sites_scs)) {
        geom_loc(
          data = sites_scs, lon = "lon", lat = "lat", crs = china_proj,
          mapping = aes(fill = .data[[col_crop]]),
          shape = 21, size = 1.5, color = "black", alpha = 0.95, linewidth = 0.2
        )
      } else {
        geom_loc(
          data = sites_scs, lon = "lon", lat = "lat", crs = china_proj,
          shape = 21, size = 1.2, fill = "red", color = "black",
          alpha = 0.95, linewidth = 0.2
        )
      }
    }
  } +
  coord_proj(crs = china_proj, xlim = scs_xlim, ylim = scs_ylim, expand = FALSE) +
  # ——加自己的经纬度网格线——
  scale_x_continuous(breaks = seq(108, 122, 4)) +
  scale_y_continuous(breaks = seq(4, 22, 6)) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "gray85", linewidth = 0.25, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text = element_text(size = 7)
  ) +
  labs(x = NULL, y = NULL)
# 组合主图+小图（右下角插入，可按需微调位置与大小）
p_final <- ggdraw() +
  draw_plot(p_main, x = 0,   y = 0,    width = 1,    height = 1) +
  draw_plot(p_scs,  x = 0.74, y = 0.08, width = 0.26, height = 0.26)

ggsave(jpg_out,  p_final, width = 8, height = 7, dpi = 600)
ggsave(png_out,  p_final, width = 8, height = 7, dpi = 600, bg = "white")
ggsave(tiff_out, p_final, width = 8, height = 7, dpi = 600, bg = "white", compression = "lzw")

message("已导出地图：", jpg_out)
message("已导出地图：", png_out)
message("已导出地图：", tiff_out)
