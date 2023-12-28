# Citi-Bike-in-Jersey-City

The trip data utilized for this project was from October 2021 to October 2023 and was retrieved from the following website: https://s3.amazonaws.com/tripdata/index.html

For the dynamic application to run seamlessly, data files and scripts must be organized in the following folder structure:


    Scripts
        Dynamic Reporting Engine - Final.Rmd
        Install_Libraries.R
        electric_vs_classic.rds
        shiny_preprocessed_rides.rds
        station_lat_lng.rds
        stations_tab.rds
    
    Data
        JC_YYYYMM_citibike_tripdata.csv

JC_202207_citibike.tripdata.csv has a typo from the website. After downloading one must manually replace citbike for citibike.


To ensure the dynamic application is user-friendly and requires minimum technical requirements, the data processing is encapsulated within the same script. Because of this, the dynamic application may take a few seconds to run, depending on the computer.
