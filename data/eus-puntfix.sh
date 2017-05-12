#/bin/bash

GR=$1

rm /tmp/puntfix*.rlx

# First, match all groups with at least 4 PUNT_something and replace with PUNTUAZIOA
cat $GR | sed -E 's/\(\"<[^\>]*>\"<PUNT_[^0-9]+(PUNT_[^0-9]+)+PUNT_[^0-9]+.*PUNT_[A-Z]+>\"\)/PUNTUAZIOA/g' > /tmp/puntfix1.rlx

# Then, replace remaining single "<x>"<PUNT_x>"s with (PUNT_x)
cat /tmp/puntfix1.rlx | \
 sed -E 's/\(\"<...>\"<PUNT_HIRU>\)/PUNT_HIRU/g' | \
# sed -E 's/\(\"<.?.>\"<PUNT_PUNT>\)/PUNT/g' | \
 sed -E 's/\(\"<.?[.;,!?]>\"<(PUNT_[A-Z_]+)>\"\)/(\1)/g' > /tmp/puntfix2.rlx


# Test that it doesn't break vislcg3
cat ~/src/cgsat/data/eus/develco14.sarrera | head -100 | vislcg3 -g /tmp/puntfix2.rlx --trace

echo "---------"
echo "Remaining punctuation, may be typos:"

grep '>"<PUNT_' /tmp/puntfix2.rlx