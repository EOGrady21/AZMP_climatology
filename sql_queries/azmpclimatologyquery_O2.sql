
SELECT MISSION_SEQ,
d.NAME,
d.DESCRIPTOR,
LEADER,
MISSION_START,
MISSION_END,
INSTITUTE,
c.name data_center,
PROTOCOL,
PLATFORM,
GEOGRAPHIC_REGION,
EVENT_SEQ,
EVENT_START,
EVENT_START_TIME,
EVENT_END,
EVENT_END_TIME,
COLLECTOR_STATION_NAME,
d.COLLECTOR_EVENT_ID,
UTC_OFFSET,
EVENT_MIN_LAT,
EVENT_MIN_LON,
EVENT_MAX_LAT,
EVENT_MAX_LON,
d.DISCRETE_SEQ,
HEADER_START,
HEADER_START_TIME,
HEADER_END,
HEADER_END_TIME,
q1.qc_definition TIME_QUALITY,
HEADER_START_LAT,
HEADER_START_LON,
HEADER_END_LAT,
HEADER_END_LON,
q2.qc_definition POSITION_QUALITY,
HEADER_START_DEPTH,
HEADER_END_DEPTH,
SOUNDING,
d.COLLECTOR_SAMPLE_ID,
COLLECTOR,
GEAR_SEQ,
GEAR_TYPE,
GEAR_MODEL,
d.DATA_TYPE_SEQ,
METHOD,
PARAMETER_NAME,
u.description unit,
d.DISCRETE_DETAIL_SEQ,
r.DATA_VALUE,
AVERAGED_DATA,
d.DATA_QC_CODE,
q.qc_definition data_quality,
r.DETECTION_LIMIT
FROM biochem.DISCRETE_DATA d , 
biochem.bcqualcodes q, biochem.bcqualcodes q1, biochem.bcqualcodes q2, biochem.bcunits u, biochem.bcdatacenters c,
biochem.bcdiscretereplicates r
where EVENT_MIN_LAT between 37 and 48
and EVENT_MIN_LON between -71 and -48
and EVENT_MAX_LAT between 37 and 48
and EVENT_MAX_LON between -71 and -48
and MISSION_START > '01-Jan-1999'
and PARAMETER_NAME = 'O2'
and r.data_qc_code = q.qc_code
and d.discrete_detail_seq = r.discrete_detail_seq
and d.time_qc_code = q1.qc_code
and d.position_qc_code = q2.qc_code
and d.converted_unit = u.unit_seq
and d.data_center_code = c.data_center_code
and d.averaged_data = 'Y'
UNION ALL
SELECT MISSION_SEQ,
d.NAME,
d.DESCRIPTOR,
LEADER,
MISSION_START,
MISSION_END,
INSTITUTE,
c.name data_center,
PROTOCOL,
PLATFORM,
GEOGRAPHIC_REGION,
EVENT_SEQ,
EVENT_START,
EVENT_START_TIME,
EVENT_END,
EVENT_END_TIME,
COLLECTOR_STATION_NAME,
d.COLLECTOR_EVENT_ID,
UTC_OFFSET,
EVENT_MIN_LAT,
EVENT_MIN_LON,
EVENT_MAX_LAT,
EVENT_MAX_LON,
d.DISCRETE_SEQ,
HEADER_START,
HEADER_START_TIME,
HEADER_END,
HEADER_END_TIME,
q1.qc_definition TIME_QUALITY,
HEADER_START_LAT,
HEADER_START_LON,
HEADER_END_LAT,
HEADER_END_LON,
q2.qc_definition POSITION_QUALITY,
HEADER_START_DEPTH,
HEADER_END_DEPTH,
SOUNDING,
d.COLLECTOR_SAMPLE_ID,
COLLECTOR,
GEAR_SEQ,
GEAR_TYPE,
GEAR_MODEL,
d.DATA_TYPE_SEQ,
METHOD,
PARAMETER_NAME,
u.description unit,
d.DISCRETE_DETAIL_SEQ,
d.DATA_VALUE,
AVERAGED_DATA,
d.DATA_QC_CODE,
q.qc_definition data_quality,
d.DETECTION_LIMIT
FROM biochem.DISCRETE_DATA d , 
biochem.bcqualcodes q, biochem.bcqualcodes q1, biochem.bcqualcodes q2, biochem.bcunits u, biochem.bcdatacenters c
where EVENT_MIN_LAT between 37 and 48
and EVENT_MIN_LON between -71 and -48
and EVENT_MAX_LAT between 37 and 48
and EVENT_MAX_LON between -71 and -48
and MISSION_START > '01-Jan-1999'
and PARAMETER_NAME = 'O2'
and d.data_qc_code = q.qc_code
and d.time_qc_code = q1.qc_code
and d.position_qc_code = q2.qc_code
and d.converted_unit = u.unit_seq
and d.data_center_code = c.data_center_code
and averaged_data = 'N'
;

