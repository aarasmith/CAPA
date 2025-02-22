



CREATE UNIQUE INDEX covering_stats
ON public.cell_stats
USING btree
(iso3n, year, month,
 capa_id_adm1, sid, cell_pop,
 month_abs, lo_25, md_25,
 hi_25, int_25, lo_50,
 md_50, hi_50, int_50,
 lo_100, md_100, hi_100,
 int_100);
 
CREATE UNIQUE INDEX covering_cell_pops
ON public.cell_pops
USING btree
(iso3n, year, capa_id_adm1, sid)
INCLUDE (cell_pop);

CREATE INDEX iso_dex ON public.cell_geos USING btree (iso3n);

CREATE UNIQUE INDEX covering_stats_yr
ON public.cell_stats_yr
USING btree
(iso3n, year, capa_id_adm1,
 sid, cell_pop, lo_25,
 lo_50, lo_100, md_25,
 md_50, md_100, hi_25,
 hi_50, hi_100, int_25,
 int_50, int_100);
 
CREATE UNIQUE INDEX covering_stats_qu
ON public.cell_stats_qu
USING btree
(iso3n, year, quarter,
 capa_id_adm1, sid,
 cell_pop, lo_25, lo_50,
 lo_100, md_25, md_50,
 md_100, hi_25, hi_50,
 hi_100, int_25, int_50,
 int_100);
 
CREATE UNIQUE INDEX covering_stats_bi
ON public.cell_stats_bi
USING btree
(iso3n, year, half,
 capa_id_adm1, sid, cell_pop,
 lo_25, lo_50, lo_100,
 md_25, md_50, md_100,
 hi_25, hi_50, hi_100,
 int_25, int_50, int_100);
