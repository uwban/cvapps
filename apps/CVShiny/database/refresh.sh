#!/bin/bash

#set cancel on error
set -e

#Variables
DB_Host="shiny.hc.local"
DB_Name="cvponl"
DB_User=""
DB_Pswd=""
SQL_Remote="/home/jhopkins/code/cvapps/apps/CVShiny/database/remote_date.sql"
SQL_Current="/home/jhopkins/code/cvapps/apps/CVShiny/database/current_date.sql"
SQL_Refresh="/home/jhopkins/code/cvapps/apps/CVShiny/database/refresh2.sql"
SQL_Refresh_temp="/home/jhopkins/code/cvapps/apps/CVShiny/database/refresh_temp.sql"
SQL_Current_Meddra="/home/jhopkins/code/cvapps/apps/CVShiny/database/current_meddra.sql"
SQL_Remote_Meddra="/home/jhopkins/code/cvapps/apps/CVShiny/database/remote_meddra.sql"

#Check database to date to see if update is necessary

export PGPASSWORD=$DB_Pswd

#get date for remote schema
psql \
    -X \
    -U $DB_User \
    -h $DB_Host \
    -f $SQL_Remote \
    -d $DB_Name > remote_date.tmp

#get date for current schema
psql \
    -X \
    -U $DB_User \
    -h $DB_Host \
    -f $SQL_Current \
    -d $DB_Name > current_date.tmp



#sed 's/-/_/g' current_date.txt > current_date.txt
#get third line of each of the temporary files
remote_date=$(sed -n '3p' remote_date.tmp)
current_date=$(sed -n '3p' current_date.tmp)

#if the remote date doesn't match execute refresh
if [ "$remote_date" = "$current_date" ]
   then
       #get the current meddra version
       psql \
          -X \
          -U $DB_User \
          -h $DB_Host \
          -f $SQL_Current_Meddra \
          -d $DB_Name > current_meddra.tmp
       psql \
          -X \
          -U $DB_User \
          -h $DB_Host \
          -f $SQL_Remote_Meddra \
          -d $DB_Name > remote_meddra.tmp

       
       current_meddra=$(sed -n '3p' current_meddra.tmp)
       remote_meddra=$(sed -n '3p' remote_meddra.tmp)

       if [ "$current_meddra" != "$remote_meddra" ]
       then
	   echo "Please update Meddra version"
       fi
       
       echo $remote_date
       #replace dashes with underscores to use as schema name in postgres
       current_date=${current_date//-/_}
       current_date=${current_date:0:11}
       current_date=${current_date// /}
       #replace placeholder terms in the sql files with values calculated from database
       sed  "s/a_date/$current_date/g" $SQL_Refresh > $SQL_Refresh_temp
       sed -i "s/a_meddra/$current_meddra/g" $SQL_Refresh_temp
       echo $current_date
       #run the edited refresh.sql script
       psql \
           -X \
           -U $DB_User \
           -h $DB_Host \
           -f $SQL_Refresh_temp \
           -d $DB_Name 

fi


