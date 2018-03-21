
ALTER SCHEMA current2 RENAME TO remote_date

CREATE SCHEMA IF NOT EXISTS current2

CREATE TABLE current2.active_ingredients AS SELECT * FROM remote.active_ingredients
CREATE TABLE current2.drug_product_ingredients AS SELECT * FROM remote.drug_product_ingredients
CREATE TABLE current2.druginvolv_lx AS SELECT * FROM remote.druginvolv_lx
CREATE TABLE current2.height_unit_lx AS SELECT * FROM remote.height_unit_lx
CREATE TABLE current2.source_lx AS SELECT * FROM remote.source_lx
CREATE TABLE current2.routeadmin_lx AS SELECT * FROM remote.routeadmin_lx
CREATE TABLE current2.frequency_lx AS SELECT * FROM remote.frequency_lx
CREATE TABLE current2.reporter_type_lx AS SELECT * FROM remote.reporter_type_lx
CREATE TABLE current2.outcome_lx AS SELECT * FROM remote.outcome_lx
CREATE TABLE current2.rptname_mv AS SELECT * FROM remote.rptname_mv
CREATE TABLE current2.adr_mv AS SELECT * FROM remote.adr_mv
CREATE TABLE current2.gender_lx AS SELECT * FROM remote.gender_lx
CREATE TABLE current2.duration_unit_lx AS SELECT * FROM remote.duration_unit_lx
CREATE TABLE current2.freq_time_unit_lx AS SELECT * FROM remote.freq_time_unit_lx
CREATE TABLE current2.record_type_lx AS SELECT * FROM remote.record_type_lx
CREATE TABLE current2.seriousness_lx AS SELECT * FROM remote.seriousness_lx
CREATE TABLE current2.drug_products AS SELECT * FROM remote.drug_products
CREATE TABLE current2.therapy_duration_unit_lx AS SELECT * FROM remote.therapy_duration_unit_lx
CREATE TABLE current2.meddra_pt_lx AS SELECT * FROM remote.meddra_pt_lx
CREATE TABLE current2.age_unit_lx AS SELECT * FROM remote.age_unit_lx
CREATE TABLE current2.dosageform_lx AS SELECT * FROM remote.dosageform_lx
CREATE TABLE current2.dose_unit_lx AS SELECT * FROM remote.dose_unit_lx
CREATE TABLE current2.meddra_pt_soc_lx AS SELECT * FROM remote.meddra_pt_soc_lx
CREATE TABLE current2.report_links AS SELECT * FROM remote.report_links
CREATE TABLE current2.report_drugs_mv AS SELECT * FROM remote.report_drugs_mv
CREATE TABLE current2.meddra_soc_lx AS SELECT * FROM remote.meddra_soc_lx
CREATE TABLE current2.report_type_lx AS SELECT * FROM remote.report_type_lx
CREATE TABLE current2.reports AS SELECT * FROM remote.reports
CREATE TABLE current2.report_drugname_mv AS SELECT * FROM remote.report_drugname_mv
CREATE TABLE current2.weight_unit_lx AS SELECT * FROM remote.weight_unit_lx
CREATE TABLE current2.report_drug AS SELECT * FROM remote.report_drug
CREATE TABLE current2.age_group_lx AS SELECT * FROM remote.age_group_lx
CREATE TABLE current2.reactions AS SELECT * FROM remote.reactions

ALTER TABLE current2.reports_table ALTER COLUMN datintreceived TYPE date

ALTER TABLE current2.reports_table ADD COLUMN age_group_clean text 

UPDATE current2.reports_table SET 
  age_group_clean = CASE 
    WHEN age_y <= 25/365 THEN 'Neonate' 
    WHEN age_y > 25/365 AND age_y < 1 THEN 'Infant' 
    WHEN age_y >= 1 AND age_y < 13 THEN 'Child' 
    WHEN age_y >= 13 AND age_y < 18 THEN 'Adolescent' 
    WHEN age_y >= 18 AND age_y <= 65 THEN 'Adult' 
    WHEN age_y > 65 THEN 'Elderly' 
    ELSE age_group_eng 'Unknown'
  END


CREATE INDEX ON current2.reports_table (seriousness_eng)
CREATE INDEX ON current2.reports_table (gender_eng)
CREATE INDEX ON current2.reports_table (height_unit_fr)
CREATE INDEX ON current2.reports_table (duration_unit_eng)
CREATE INDEX ON current2.reports_table (report_type_code)
CREATE INDEX ON current2.reports_table (seriousness_fr)
CREATE INDEX ON current2.reports_table (height_unit_eng)
CREATE INDEX ON current2.reports_table (reporter_type_eng)
CREATE INDEX ON current2.reports_table (age_unit_eng)
CREATE INDEX ON current2.reports_table (reporter_type_fr)
CREATE INDEX ON current2.reports_table (age_unit_fr)
CREATE INDEX ON current2.reports_table (outcome_eng)
CREATE INDEX ON current2.reports_table (datintreceived)
CREATE INDEX ON current2.reports_table (datreceived)
CREATE INDEX ON current2.reports_table (age_group_code)
CREATE INDEX ON current2.reports_table (age_group_eng)
CREATE INDEX ON current2.reports_table (death)
CREATE INDEX ON current2.reports_table (pt_name_fr)
CREATE INDEX ON current2.reports_table (age_group_clean)
CREATE INDEX ON current2.reports_table (height)
CREATE INDEX ON current2.reports_table (report_type_eng)
CREATE INDEX ON current2.reports_table (reporter_type_code)
CREATE INDEX ON current2.reports_table (version_no)
CREATE INDEX ON current2.reports_table (gender_code)
CREATE INDEX ON current2.reports_table (report_type_fr)
CREATE INDEX ON current2.reports_table (age_y)
CREATE INDEX ON current2.reports_table (life_threatening)
CREATE INDEX ON current2.reports_table (drugname)
CREATE INDEX ON current2.reports_table (congenital_anomaly)
CREATE INDEX ON current2.reports_table (report_link_flg)
CREATE INDEX ON current2.reports_table (pt_name_eng)
CREATE INDEX ON current2.reports_table (duration)
CREATE INDEX ON current2.reports_table (weight_unit_code)
CREATE INDEX ON current2.reports_table (disability)
CREATE INDEX ON current2.reports_table (soc_name_eng)
CREATE INDEX ON current2.reports_table (outcome_fr)
CREATE INDEX ON current2.reports_table (weight)
CREATE INDEX ON current2.reports_table (other_medically_imp_cond)
CREATE INDEX ON current2.reports_table (height_unit_code)
CREATE INDEX ON current2.reports_table (mah_no)
CREATE INDEX ON current2.reports_table (source_fr)
CREATE INDEX ON current2.reports_table (gender_fr)
CREATE INDEX ON current2.reports_table (weight_unit_fr)
CREATE INDEX ON current2.reports_table (source_code)
CREATE INDEX ON current2.reports_table (source_eng)
CREATE INDEX ON current2.reports_table (seriousness_code)
CREATE INDEX ON current2.reports_table (soc_name_fr)
CREATE INDEX ON current2.reports_table (duration_unit_fr)
CREATE INDEX ON current2.reports_table (age)
CREATE INDEX ON current2.reports_table (outcome_code)
CREATE INDEX ON current2.reports_table (weight_unit_eng)
CREATE INDEX ON current2.reports_table (report_id)
CREATE INDEX ON current2.reports_table (hosp_required)
CREATE INDEX ON current2.reports_table (report_no)
CREATE INDEX ON current2.reports_table (age_unit_code)
CREATE INDEX ON current2.reports_table (age_group_fr)
CREATE INDEX ON current2.reports_table (aer_id)

CREATE INDEX ON current2.report_drug (routeadmin_code)
CREATE INDEX ON current2.report_drug (indication_name_eng)
CREATE INDEX ON current2.report_drug (routeadmin_eng)
CREATE INDEX ON current2.report_drug (indication_name_fr)
CREATE INDEX ON current2.report_drug (unit_dose_qty)
CREATE INDEX ON current2.report_drug (freq_time_unit_code)
CREATE INDEX ON current2.report_drug (seq_product)
CREATE INDEX ON current2.report_drug (frequency_time_eng)
CREATE INDEX ON current2.report_drug (therapy_duration_unit_code)
CREATE INDEX ON current2.report_drug (seq_therapy)
CREATE INDEX ON current2.report_drug (druginvolv_fr)
CREATE INDEX ON current2.report_drug (routeadmin_fr)
CREATE INDEX ON current2.report_drug (product_id)
CREATE INDEX ON current2.report_drug (dosageform_code)
CREATE INDEX ON current2.report_drug (dosageform_eng)
CREATE INDEX ON current2.report_drug (drug_product_id)
CREATE INDEX ON current2.report_drug (therapy_duration_unit_fr)
CREATE INDEX ON current2.report_drug (druginvolv_code)
CREATE INDEX ON current2.report_drug (freq_time_unit_fr)
CREATE INDEX ON current2.report_drug (therapy_duration_unit_eng)
CREATE INDEX ON current2.report_drug (report_id)
CREATE INDEX ON current2.report_drug (dose_unit_fr)
CREATE INDEX ON current2.report_drug (frequency)
CREATE INDEX ON current2.report_drug (report_drug_id)
CREATE INDEX ON current2.report_drug (dose_unit_code)
CREATE INDEX ON current2.report_drug (dose_unit_eng)
CREATE INDEX ON current2.report_drug (therapy_duration)
CREATE INDEX ON current2.report_drug (aer_id)
CREATE INDEX ON current2.report_drug (drugname)
CREATE INDEX ON current2.report_drug (dosageform_fr)
CREATE INDEX ON current2.report_drug (freq_time_unit_eng)
CREATE INDEX ON current2.report_drug (freq_time)
CREATE INDEX ON current2.report_drug (frequency_time_fr)
CREATE INDEX ON current2.report_drug (druginvolv_eng)

CREATE INDEX ON current2.drug_product_ingredients (product_id)
CREATE INDEX ON current2.drug_product_ingredients (active_ingredient_id)
CREATE INDEX ON current2.drug_product_ingredients (drug_product_id)
CREATE INDEX ON current2.drug_product_ingredients (drug_product_ingredient_id)
CREATE INDEX ON current2.drug_product_ingredients (active_ingredient_name)
CREATE INDEX ON current2.drug_product_ingredients (drugname)

CREATE INDEX ON meddra.v_20_1 (pt_code)
CREATE INDEX ON meddra.v_20_1 (soc_name_fr)
CREATE INDEX ON meddra.v_20_1 (soc_name_eng)
CREATE INDEX ON meddra.v_20_1 (hlt_code)
CREATE INDEX ON meddra.v_20_1 (pt_soc_code)
CREATE INDEX ON meddra.v_20_1 (pt_name_fr)
CREATE INDEX ON meddra.v_20_1 (reaction_id)
CREATE INDEX ON meddra.v_20_1 (report_id)
CREATE INDEX ON meddra.v_20_1 (pt_name_eng)
CREATE INDEX ON meddra.v_20_1 (soc_code)
CREATE INDEX ON meddra.v_20_1 (smq_code)
CREATE INDEX ON meddra.v_20_1 (hlt_name)
CREATE INDEX ON meddra.v_20_1 (smq_name)