#!/bin/csh -f
 
#  This script generates input files for processing Bluesky smoke dispersion datasets as distributed tasks
#  These files are used to set up an array job by using the filenames in the SGE_TASK_ID parameter
#  Specify the name of the datset to be called as the content of the file and the name of the file 
#  to be continuous over the interval. max = #years_of_data * #_of_months

set i = 1
set max = 12
while ($i <= $max)
   foreach yyyy (2014 2015 2016 2017 2018 2019)
     foreach mm (01 02 03 04 05 06 07 08 09 10 11 12)
            if ($i < 10 ) then
                  echo "${yyyy}${mm}" > bluesky0${i}
            else
                  echo "${yyyy}${mm}" > bluesky${i}
            endif 
        @ i++
     end
   end
end
