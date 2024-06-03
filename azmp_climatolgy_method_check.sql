select parameter_name,
count(*) as count
from biochem.discrete_data
where EVENT_MIN_LAT between 37 and 48
and EVENT_MIN_LON between -71 and -48
and EVENT_MAX_LAT between 37 and 48
and EVENT_MAX_LON between -71 and -48
and MISSION_START > '01-Jan-1999'
group by parameter_name
order by count desc;