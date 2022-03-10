#!/bin/bash

# The URL https://www.spinellis.gr/cgi-bin/oasa-history?id=YOUR_STUDENT_ID (e.g. https://www.spinellis.gr/cgi-bin/oasa-history?id=x2899999) 
# returns a constantly updated stream of GPS location records associated with public buses in Athens. 
# The data (more than 560MB when uncompressed) are provided in a compressed format compatible with the gzip program. 
# The stream's fields are: data acquisition time stamp, line number, bus number, position reporting time stamp, bus position latitude, bus position longitude.

# Your mission, should you decide to accept it, is to analyze the data in order to help the bus company improve its operations, and thus improve everyday life in the city. 
# Answer the following questions using Unix command-line filter tools, such as grep, fgrep, wc, sort, cut, tee, xargs, uniq, awk, sed, join, comm, diff, seq, head, tail, rev, tac, gzip, tr, 
# tsort, etc.  For each question provide the answer and the command(s) you used to obtain it.  For multi-stage pipelines, add comments on a line before each
# command or after the command's name to explain what your commands do. (On the Unix shell comments start with the # character.)
# All commands given must produce exactly the answer you provide. (It is not allowed to interpret the data manually or with other tools.)

# You are allowed (and encouraged) to consult command manual pages, course material, and Wikipedia.
# Given the data's large size you might want to first try out your solutions on a small subset of the data until you're satisfied with the output.

# For all answers use the same version of the data file, which you will store locally (perhaps compressed) on the computer you use.
# Ensure that you provide your correct student-id in the URL you use for obtaining the data.  The deadline for finishing this exercise is March 15th, anytime on Earth.
# Do not answer any questions for which you cannot deduce your answer on your own.  Note that you may be asked to repeat and explain your answers in an oral interview.
# Any violation of these rules or attempt to circumvent them will result in a grade of zero.
# At the end of this exercise you will be asked to indicate how many hours it took you to complete it and provide feedback on the exercise and the lectures. Good luck!

# As always, should you be caught or killed while working on this mission, the faculty will disavow any knowledge of your actions.  This page will not self-destruct in ten seconds.


curl https://www.spinellis.gr/cgi-bin/oasa-history?id=p2822124 -o oasa.gz
gunzip oasa.gz 
head -100 oasa > oasa.trimmed


# test for delimeter
head -1 oasa.trimmed | xargs echo  | cut -d ";" -f 1
head -1 oasa.trimmed | xargs echo  | cut -d " " -f 1
head -1 oasa.trimmed | xargs echo  | cut -d "," -f 1

head -1 oasa.trimmed

# How many records are provided with the data?
wc -l < oasa

# What is the data acquisition time stamp of the last record in the stream you fetched?
awk -F ',' '{print $1}' oasa | sort -d -r | head -1

# How many different buses appear in the data?

# slow performance
awk -F ',' '{print $3}' oasa | sort | uniq -u | wc -l
awk -F ',' '{print $3}' oasa | sort -u | wc -l

# optimal
sort -u -t "," -k 3n  oasa | wc -l
awk -F ',' '!a[$3]++' oasa | wc -l

# How many different routes are covered by the data?

# using optimal from previous commands
sort -u -t "," -k 2n  oasa | wc -l

# How many dates are covered by the data?
awk -F 'T' '!a[$1]++' oasa | wc -l

# Which route is associated with the most position reports?

awk -F ',' '{if(count[$2]++ >= max) max = count[$2]} END {for ( i in count ) if(max == count[i]) print i, count[i] }' oasa
# line 3609 count 109781

awk -F, '{print $2}' oasa | sort -n | uniq -c | sort -k 1nr | head -1

# How many position reports appear in the data more than once?
awk -F "," '{print $5, $6}' oasa | sort | uniq -d | wc -l

# Which is the most frequent first two-digit sequence in numbers assigned to buses?

awk -F ',' '{print substr($3,0,2) "\t" $3}' oasa | awk '{if(count[$1]++ >= max) max = count[$1]} END {for ( i in count ) if(max == count[i]) print i, count[i] }'

# How many buses did not travel on this year's February 6th?

awk -F ',' '{print $3}' oasa | sort -k 1n | uniq > total
grep "Feb  6 2022" oasa | awk -F ',' '{print $3}' | sort -k 1n | uniq > in_Feb6
comm -3 total in_Feb6 | wc -l
rm -rf total in_Feb6

# On which date were the most buses on the road?

date -d "$(awk -F ',' '{print $3";"substr($4,1,11)}' oasa | sort -u -d | awk -F ';' '{print $2}' | sort -d | uniq -c | sort -k 1nr | head -1 | cut -d' ' -f5-)" +%Y-%m-%d

# Which route has been served by the highest number of different buses?

awk -F ',' '{print $2","$3, $2, $3}' oasa | awk '!a[$1]++' | awk '{print $2 }' | sort -k 1n | uniq -c | sort -k 1nr | head -n1

# On which hour of the day (e.g. 09) are there overall the most buses on the road?

awk -F ',' '{print $3, substr($4, 13, 2)""substr($4, length($4)-1, 2)}' oasa | sort -d | uniq | awk '{print $2}' | sort -k 1n | uniq -c | sort -k 1nr | head -n 1

# On which hour of the day (e.g. 09) are there overall the least buses on the road?

awk -F ',' '{print $3, substr($4, 13, 2)""substr($4, length($4)-1, 2)}' oasa | sort -d | uniq | awk '{print $2}' | sort -k 1n | uniq -c | sort -k 1n | head -n 1

# For which weekday (e.g. Wednesday) does your data set contain the most records?
(awk -F ',' '{print $4}' oasa | awk -F ' ' '{print $1, $2, $3}' > out) && (date -f out +%A | sort | uniq -c | sort -k 1n | tail -1) && (rm -rf out)

# What are the bounding box geographic coordinates of the area served by the buses?

awk -F ',' '{print $5}' oasa | sort -k 1n | tail -1

awk -F ',' '{print $5}' oasa | sort -k 1n | head -1

awk -F ',' '{print $6}' oasa | sort -k 1n | tail -1

awk -F ',' '{print $6}' oasa | sort -k 1n | head -1

# Which bus has appeared closest to your favorite location?

# 37.894846,23.877210


fav_lat=37.894846
fav_lon=23.877210

awk -F ',' '
{
  d=sqrt(($fav_lat-$5)^2 + ($fav_lot-$6)^2)
  print $3, $5, $6, d
}
' oasa | sort -u -k 4n | uniq | tail -1 | echo $1

# How many position reports have been sent by the chosen bus?

selected_bus=$(awk -F ',' '
{
  d=sqrt(($fav_lat-$5)^2 + ($fav_lot-$6)^2)
  print $3, $5, $6, d
}
' oasa | sort -u -k 4n | uniq | tail -1 | cut -f 1 -d " ")

awk -F ',' -v bus="$selected_bus" '{if($3==bus) {print}}' oasa | wc -l

# What was the chosen bus's last position in the obtained data stream?

awk -F ',' -v bus="$selected_bus" '{if($3==bus) {print $1, $5, $6}}' oasa | sort -d -r -k 1 | head -1

# On which date has the chosen bus given the most position reports?

awk -F ',' -v bus="$selected_bus" '{if($3==bus) {print $4}}' oasa | awk -F ' ' '{print $1, $2, $3}' | sort -d -k 1 | uniq -c | sort -k 1n | tail -1 | xargs

# On how many routes has the chosen bus traveled?

awk -F ',' -v bus="$selected_bus" '{if($3==bus) {print $2}}' oasa | sort -k 1n -u | wc -l

# How many buses have shared at least one route with the chosen bus?

awk -F ',' -v bus="$selected_bus" '{if($3==bus) {print $2$3, $2, $3}}' oasa | awk '!a[$1]++' | awk '{print $2}' | sort -t',' -k 1n > right
awk -F ',' -v bus="$selected_bus" '{if($3!=bus) {print $2$3, $2, $3}}' oasa | awk '!a[$1]++' | awk '{print $2","$3}' | sort -t',' -k 1n > left
join -t ',' -11 -21 left right | awk -F, '!a[$2]++' | wc -l
rm -rf left right