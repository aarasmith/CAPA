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
