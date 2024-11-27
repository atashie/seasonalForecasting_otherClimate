#####################################
#####################################
#####################################
## STREAMFLOW PROCESSING CODE
#####################################
#####################################
#####################################

# Define directories
npy_dir = 'forecasts/npy_files'
h5_dir = 'forecasts/h5_files'
plot_dir = 'forecasts/plots'

# Create directories if they don't exist
os.makedirs(npy_dir, exist_ok=True)
os.makedirs(h5_dir, exist_ok=True)
os.makedirs(plot_dir, exist_ok=True)

##########################
# Part 0a: Select the location and data

thisRow = -1
while True:
    thisRow += 1
    start_date = start_date_og
    end_date = end_date_og

    if use_webdata:
        s3_tableName = "DIU_locations_Sep2024.csv"
        my_df = read_in_dataframe(s3_folder, s3_tableName)
        if thisRow > my_df.shape[0]:
            print("all locations have been modeled, stopping routine")
            break
            # Skip row if Data_Loc_EX is NaN
        if pd.isna(my_df["Data_Loc_EX"].iloc[thisRow]):
            thisRow += 1
            # Second check after incrementing thisRow
            if thisRow >= my_df.shape[0]:
                print("all locations have been modeled, stopping routine")
                break
            continue
        lat, lon = my_df["Lat"].loc[thisRow], my_df["Lon"].loc[thisRow]
        location_name = my_df["Location_Name"].loc[thisRow]
        streamflow_df = process_webdata(row=my_df.loc[thisRow,:], start_date=start_date, end_date=end_date)
        print(streamflow_df)
        streamflow_df_histOnly = process_webdata(row=my_df.loc[thisRow,:], start_date=start_date_all, end_date=end_date)
        streamflow_df_histOnly.to_csv(f'{npy_dir}/{location_name}_historicalStreamflow.csv', index=False)
        
    if use_csvs:
        s3_tableName = "CustomerOnboardingTemplate_Simplot2024.csv"
        my_df = read_in_dataframe(s3_folder, s3_tableName)
        if thisRow > my_df.shape[0]:
            print("all locations have been modeled, stopping routine")
            break
        if not my_df["Is_Unique"].iloc[thisRow]:
            thisRow += 1
            # Second check after incrementing thisRow
            if thisRow >= my_df.shape[0]:
                print("all locations have been modeled, stopping routine")
                break
            continue
        lat, lon = my_df["Lat"].loc[thisRow], my_df["Lon"].loc[thisRow]
        location_name = my_df["File_name"].loc[thisRow]
        streamflow_df, _, _ = process_usace_data(s3_folder, location_name, start_date=start_date, end_date=end_date)
        print(streamflow_df)

        # redefining start / end date based on data availability
    start_date, end_date = streamflow_df['Date'].min().strftime('%Y/%m/%d'), streamflow_df['Date'].max().strftime('%Y/%m/%d')

    target_data = streamflow_df[["Value"]].values.astype("float32")
    lagged_data = create_lagged_feature(target_data, hacky_lookahead)  
    target_data = np.concatenate((target_data, lagged_data), axis = 1)

    print(f"finished processing streamflow data for {location_name}")
    print(target_data)
    isNow = datetime.now()
    print(f"{location_name} date and time is: {isNow}")
    output_gpkg_s3_path = f's3://climate-ai-data-science-datasets/arrakis-data/climate-data-aggregation/geopackages/merged_subwatersheds/{location_name}.gpkg'

    
    
#####################################
#####################################
#####################################
## BASIN AND CLIMATE PROCESSING CODE
#####################################
#####################################
#####################################

    # Part 0b: Generate new basin shapes and save to GeoPackage
       # completed during model tuning
    
    # Part 1: tune model completed
    # Part 2: feature selection completed

#####################################
#####################################
#####################################
## OPERATIONAL FORECAST
#####################################
#####################################
#####################################    

    # Part 3a: process climate data forecasts
    result, date_range = process_climate_data(variables, start_date, end_date, location_name, streamflow_df, hacky_lookahead, output_gpkg_s3_path)
    
    print(f"Shape of each member's data: {next(iter(result.values())).shape}")
    print(f"Date range: from {date_range[0]} to {date_range[-1]}")

    ######################
    ### Part 3b: operational forecasts

    study_path = f"data/tuned_models/study_test_{location_name}.pkl"
    climate_data_dir = f"data/operationalForecast/ClimateData_{location_name}"

    forecast_mean, forecast_lower, forecast_upper, ensemble_forecast = run_operational_forecast(location_name, study_path, climate_data_dir)
    
    ######################
    ### Part 3c: inspect operational forecasts
    
    # Reshape forecast arrays if needed
    forecast_mean = forecast_mean.reshape(-1)
    forecast_lower = forecast_lower.reshape(-1)
    forecast_upper = forecast_upper.reshape(-1)

    # Create a date range for the forecast
    forecast_dates = pd.date_range(end=date_range[-1],# + pd.Timedelta(days=hacky_lookahead), 
                                   periods=len(forecast_mean))

    # Plot the ensemble forecasts
    plt.figure(figsize=(12, 6))
    for i in range(ensemble_forecast.shape[0]):
        plt.plot(forecast_dates, ensemble_forecast[i, :, 0], alpha=0.1, color='red')

    plt.fill_between(forecast_dates, forecast_lower, forecast_upper, color='blue', alpha=0.35, label='90% Confidence Interval')
    plt.plot(forecast_dates, forecast_mean, label='Mean Forecast', color='red', linewidth=2)
    plt.plot(streamflow_df["Date"], streamflow_df["Value"], color='black', label='Historical Data')

    # Set limits, legend, and labels
    y_min = min(streamflow_df["Value"].min(), forecast_lower.min())
    y_max = max(streamflow_df["Value"].max(), forecast_upper.max())
    plt.ylim(y_min, y_max)
    plt.legend()
    plt.title('Ensemble Forecast')
    plt.xlabel('Date')
    plt.ylabel('Streamflow')
    plt.gcf().autofmt_xdate()

    # Save the plot
    plt.savefig(f"{plot_dir}/ensemble_forecast_{location_name}_{forecast_dates[0].date()}.png")
    plt.close()  # Close the plot to avoid displaying it here

    # Save the ensemble_forecast to .npy and .h5 files
    saveFileString = f"{location_name}_{forecast_dates[0]}"
    np.save(f"{npy_dir}/{saveFileString}.npy", ensemble_forecast)
    
    with h5py.File(f'{h5_dir}/{saveFileString}.h5', 'w') as f:
        f.create_dataset('array', data=ensemble_forecast)

    print(f"Saved files for {location_name} in {npy_dir} and {h5_dir} directories.")
    