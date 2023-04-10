cell_stats_schema <- "CREATE TABLE cell_stats (
sid BIGINT,
month_abs SMALLINT,
iso3n SMALLINT,
capa_id_adm1 BIGINT,
year SMALLINT,
month SMALLINT,
cell_pop INT,
lo_25 SMALLINT,
md_25 SMALLINT,
hi_25 SMALLINT,
int_25 INT,
lo_50 SMALLINT,
md_50 SMALLINT,
hi_50 SMALLINT,
int_50 INT,
lo_100 SMALLINT,
md_100 SMALLINT,
hi_100 SMALLINT,
int_100 INT
)"

cell_pops_schema <- "CREATE TABLE cell_pops1 (
sid BIGINT,
iso3n SMALLINT,
capa_id_adm1 BIGINT,
year SMALLINT,
cell_pop INT
)"

cell_geos_schema <- "CREATE TABLE cell_geos (
sid BIGINT,
gid SMALLINT,
iso3n SMALLINT,
geometry GEOMETRY
)"

grid_geos_schema <- "CREATE TABLE grid_geos (
gid SMALLINT,
iso3n SMALLINT,
geometry GEOMETRY
)"

region_key_schema <- "CREATE TABLE region_key (
iso3n SMALLINT,
region TEXT
)"

region_pops_schema <- "CREATE TABLE region_pops (
region TEXT,
year SMALLINT,
total_pop BIGINT
)"

period_schema <- "CREATE TABLE period (
month SMALLINT,
quarter SMALLINT,
half SMALLINT
)"


# cell_stats_yr_schema <- "CREATE TABLE cell_stats (
# sid BIGINT,
# iso3n SMALLINT,
# capa_id_adm1 BIGINT,
# year SMALLINT,
# cell_pop INT,
# lo_25 SMALLINT,
# md_25 SMALLINT,
# hi_25 SMALLINT,
# int_25 INT,
# lo_50 SMALLINT,
# md_50 SMALLINT,
# hi_50 SMALLINT,
# int_50 INT,
# lo_100 SMALLINT,
# md_100 SMALLINT,
# hi_100 SMALLINT,
# int_100 INT
# )"

country_pops_schema <- "CREATE TABLE country_pops (
iso3n SMALLINT,
year SMALLINT,
total_pop BIGINT
)"

# cell_stats_yr_view <- "CREATE MATERIALIZED VIEW cell_stats_yr AS
#   SELECT
#     sid, iso3n, capa_id_adm1, year, SUM(cell_pop) AS cell_pop,
#     MAX(lo_25) as lo_25,
#     MAX(lo_50) as lo_50,
#     MAX(lo_100) as lo_100,
#     MAX(md_25) as md_25,
#     MAX(md_50) as md_50,
#     MAX(md_100) as md_100,
#     MAX(hi_25) as hi_25,
#     MAX(hi_50) as hi_50,
#     MAX(hi_100) as hi_100,
#     MAX(int_25) as int_25,
#     MAX(int_50) as int_50,
#     MAX(int_100) as int_100
#   FROM 
#     (
#     SELECT 
#       sid, iso3n, capa_id_adm1, year,
#       MAX(cell_pop) as cell_pop,
#       SUM(lo_25) as lo_25,
#       SUM(lo_50) as lo_50,
#       SUM(lo_100) as lo_100,
#       SUM(md_25) as md_25,
#       SUM(md_50) as md_50,
#       SUM(md_100) as md_100,
#       SUM(hi_25) as hi_25,
#       SUM(hi_50) as hi_50,
#       SUM(hi_100) as hi_100,
#       SUM(int_25) as int_25,
#       SUM(int_50) as int_50,
#       SUM(int_100) as int_100 
#     FROM cell_stats
#     GROUP BY iso3n, capa_id_adm1, year, sid
#     ) agg
#   GROUP BY iso3n, capa_id_adm1, year, sid"

month_key <- data.frame(month = 1:12, quarter = rep(1:4, each = 3), half = rep(1:2, each = 6))
dbWriteTable(capa_db, "month_key", month_key)

cell_stats_qu_view <- "CREATE MATERIALIZED VIEW cell_stats_qu AS
  SELECT 
      sid, iso3n, capa_id_adm1, year, quarter,
      MAX(cell_pop) as cell_pop,
      SUM(lo_25) as lo_25,
      SUM(lo_50) as lo_50,
      SUM(lo_100) as lo_100,
      SUM(md_25) as md_25,
      SUM(md_50) as md_50,
      SUM(md_100) as md_100,
      SUM(hi_25) as hi_25,
      SUM(hi_50) as hi_50,
      SUM(hi_100) as hi_100,
      SUM(int_25) as int_25,
      SUM(int_50) as int_50,
      SUM(int_100) as int_100 
    FROM 
        (SELECT * FROM
         cell_stats) stats
        LEFT JOIN
        month_key
        ON stats.month = month_key.month
    GROUP BY iso3n, capa_id_adm1, year, quarter, sid"

cell_stats_bi_view <- "CREATE MATERIALIZED VIEW cell_stats_bi AS
  SELECT 
      sid, iso3n, capa_id_adm1, year, half,
      MAX(cell_pop) as cell_pop,
      SUM(lo_25) as lo_25,
      SUM(lo_50) as lo_50,
      SUM(lo_100) as lo_100,
      SUM(md_25) as md_25,
      SUM(md_50) as md_50,
      SUM(md_100) as md_100,
      SUM(hi_25) as hi_25,
      SUM(hi_50) as hi_50,
      SUM(hi_100) as hi_100,
      SUM(int_25) as int_25,
      SUM(int_50) as int_50,
      SUM(int_100) as int_100 
    FROM 
        (SELECT * FROM
         cell_stats) stats
        LEFT JOIN
        month_key
        ON stats.month = month_key.month
    GROUP BY iso3n, capa_id_adm1, year, half, sid"

cell_stats_yr_view <- "CREATE MATERIALIZED VIEW cell_stats_yr AS
  SELECT 
      sid, iso3n, capa_id_adm1, year,
      MAX(cell_pop) as cell_pop,
      SUM(lo_25) as lo_25,
      SUM(lo_50) as lo_50,
      SUM(lo_100) as lo_100,
      SUM(md_25) as md_25,
      SUM(md_50) as md_50,
      SUM(md_100) as md_100,
      SUM(hi_25) as hi_25,
      SUM(hi_50) as hi_50,
      SUM(hi_100) as hi_100,
      SUM(int_25) as int_25,
      SUM(int_50) as int_50,
      SUM(int_100) as int_100 
    FROM 
        cell_stats
    GROUP BY iso3n, capa_id_adm1, year, sid"

region_pops_view <- "CREATE MATERIALIZED VIEW region_pops AS
  SELECT
      region, year, sum(total_pop) AS total_pop
  FROM
    country_pops
  LEFT JOIN
    region_key
  ON
    country_pops.iso3n = region_key.iso3n
  GROUP BY region, year
  ORDER BY region, year"

adm1_pops_view <- "CREATE MATERIALIZED VIEW adm1_pops AS
  SELECT
    iso3n, capa_id_adm1, year, sum(cell_pop) AS total_pop
  FROM
      cell_pops
  WHERE
      capa_id_adm1 is not null
  GROUP BY
      iso3n, capa_id_adm1, year"
