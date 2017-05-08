#/bin/bash

cat /tmp/eus.readings.new | tr ',' ' ' | tr -d '()' | tr -s ' ' | sed -E 's/ /></g ; s/^(.*)$/<\1>/ ; s/<>//g' > eus.readings
