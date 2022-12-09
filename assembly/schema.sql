CREATE TABLE grid_geos (
gid SMALLINT,
iso3n SMALLINT,
geometry GEOMETRY,
primary key (iso3n, gid)
);

CREATE TABLE cell_geos (
sid BIGINT primary key,
gid SMALLINT,
iso3n SMALLINT,
geometry GEOMETRY,
foreign key (iso3n, gid) references grid_geos (iso3n, gid)
);

CREATE TABLE cell_pops (
sid BIGINT,
iso3n SMALLINT,
capa_id_adm1 BIGINT,
year SMALLINT,
cell_pop INT,
primary key (sid, year),
foreign key (sid) references cell_geos (sid)
);

CREATE TABLE cell_stats (
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
int_100 INT,
primary key (sid, year, month),
foreign key (sid) references cell_geos (sid),
foreign key (sid, year) references cell_pops (sid, year)
);

CREATE TABLE region_key (
iso3n SMALLINT,
region TEXT,
primary key (iso3n, region)
);

CREATE TABLE month_key (
month SMALLINT,
quarter SMALLINT,
half SMALLINT,
primary key (month)
);

CREATE TABLE country_pops (
iso3n SMALLINT,
year SMALLINT,
total_pop BIGINT,
primary key (iso3n, year)
);

CREATE MATERIALIZED VIEW cell_stats_qu AS
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
    GROUP BY iso3n, capa_id_adm1, year, quarter, sid;

CREATE MATERIALIZED VIEW cell_stats_bi AS
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
    GROUP BY iso3n, capa_id_adm1, year, half, sid;

CREATE MATERIALIZED VIEW cell_stats_yr AS
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
    GROUP BY iso3n, capa_id_adm1, year, sid;

CREATE MATERIALIZED VIEW region_pops AS
  SELECT
      region, year, sum(total_pop) AS total_pop
  FROM
    country_pops
  LEFT JOIN
    region_key
  ON
    country_pops.iso3n = region_key.iso3n
  GROUP BY region, year
  ORDER BY region, year;

CREATE MATERIALIZED VIEW adm1_pops AS
  SELECT
    iso3n, capa_id_adm1, year, sum(cell_pop) AS total_pop
  FROM
      cell_pops
  WHERE
      capa_id_adm1 is not null
  GROUP BY
      iso3n, capa_id_adm1, year;
