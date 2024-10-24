variables = ['t2m_max', 'tp', 't2m_min']
era5_path = 's3://climate-ai-data-eng-prod-processed/era5/v1/daily.zarr/'
seas5_path = 's3://climate-ai-data-eng-prod-processed/seas5/v1/rev4/daily_debiased.zarr/'
start_date = '2020/01/01'
csv_file = "data/Mississippi River _at_ Arkansas City_ AR.csv"

hacky_lookahead = 180

lat = 33.5515
lon = -91.2384

offsets = [(3, 3), (3, -3.24), (0.5,0)]
feature_lags = [0,0,0,0,0,0,0,0]
#recent_date 



# 
def process_data_for_operational_forecasts(streamflow_path, era5_path, seas5_path, variables, lat, lon, offsets, start_date = '2020/01/01', feature_lags = [0,0,0,0,0,0]):
    Path("data/operationalForecastClimateData").mkdir(parents=True, exist_ok=True)
    
    lats = [lat + offset[0] for offset in offsets]
    lons = [lon + offset[1] for offset in offsets]

    era5_all = xr.open_zarr(era5_path, consolidated=True)
    era5_sub = era5_all.sel(time = slice(start_date, None))
    seas5_all = xr.open_zarr(seas5_path, consolidated=True)



    df = pd.read_csv(csv_file, parse_dates=True)#, index_col="Date")
    # sort dates
    df['Date'] = pd.to_datetime(df['Date'])
    df['Date'] = df['Date'].dt.date
    df = df.sort_values(by='Date')
    start_date = df['Date'].min().strftime('%Y/%m/%d')

    # clean stage data
    df['Stage'] = df['Stage'].apply(lambda x: float(x.replace(',', '')))
    df['Stage'] = df['Stage'].astype(float)
    while any(df['Stage'] < -999):
        mask = df['Stage'] < -999
        df.loc[mask, 'Stage'] = df['Stage'].shift(1)

    # Check for missing dates and interpolate, if needed
    date_range = pd.date_range(start=min(df['Date']), end=max(df['Date'])).date
    missing_dates = [date for date in date_range if date not in df['Date'].values]
    if missing_dates:
        print(f"There are {len(missing_dates)} missing dates.")
        #print(missing_dates)
        # Create a new DataFrame with complete date range
        date_range_df = pd.DataFrame(date_range, columns=['Date'])
        df = pd.merge(date_range_df, df, on='Date', how='left')
        df.set_index('Date', inplace=True)
        # Perform linear interpolation
        df.interpolate(method='linear', inplace=True)
        # Reset the index
        df.reset_index(inplace=True)
        print(f"Missing dates have been resolved by linear interpolation")
    else:
        print("There are no missing dates")

    # identify start / stop
    start_date = df['Date'].min().strftime('%Y/%m/%d')
    end_date = df['Date'].max().strftime('%Y/%m/%d')

    target_data = df[["Stage"]].values.astype("float32")
    lagged_data = create_lagged_feature(target_data, hacky_lookahead)  
    target_data = np.concatenate((target_data, lagged_data), axis = 1)
    # shift the stage feature
    new_df = df.copy()
    new_df["Date"] = new_df["Date"] + pd.Timedelta(days=hacky_lookahead)
    new_df['Date'] = pd.to_datetime(new_df['Date'])
    stage_mean = new_df['Stage'].mean()


    for thisMember in seas5_all.member.values:
        print(f"starting run for member {thisMember}")
        seas5_sub = seas5_all.sel(member = thisMember)
        
        timeseries = target_data
        target_data = df[["Stage"]].values.astype("float32")
        
        for thisCount in range(len(lats)):
            print(f"lat count {thisCount}")
            lat = lats[thisCount]
            lon = lons[thisCount]
            era5 = era5_sub.sel(
                        latitude=lat,
                        longitude=lon,
                        method='nearest')
            seas5 = seas5_sub.sel(init_time=seas5_sub.init_time[-1],
                          latitude=lat,
                          longitude=lon,
                          method='nearest')
            
            seas5 = seas5.assign_coords({'time': seas5.init_time + seas5.lead_time})
            seas5 = seas5.drop_vars('lead_time')
            seas5 = seas5.rename({'lead_time': 'time'})
            seas5 = seas5.drop_vars('init_time')
            seas5 = seas5.set_index(time='time')


            last_era5_time = era5.time[-1].values
            seas5_sel = seas5.sel(time=slice(last_era5_time, None))
            merged_ds = xr.concat([era5,seas5_sel], dim='time')
            
            
            
            for index, var in enumerate(variables):
                print(index, var)
                data_ds_df = merged_ds[var].to_dataframe()
    #            data_ds_df['time'] = pd.to_datetime(data_ds_df['time'])
                #print(data_ds_df)
    #            merged_ds[var].values
        
                merged_df = pd.merge(new_df, data_ds_df, left_on='Date', right_on='time', how='right')
                #print(merged_df)
                columns_from_new_df = new_df.columns.difference(data_ds_df.columns)
                merged_df[columns_from_new_df] = merged_df[columns_from_new_df].fillna(stage_mean)
                merged_df["Date"] = merged_ds.time.values
                #print(merged_df)
                if index == 0 and thisCount == 0:
                    print(f"initializing new timeseries for member {member}")
                    timeseries = merged_df["Stage"].values.reshape(-1, 1)
                last_column = merged_df.iloc[:, -1].values.reshape(-1, 1)
                timeseries = np.concatenate((timeseries, last_column), axis=1)
                print(timeseries.shape)
            
            np.save(f"data/operationalForecastClimateData/data_{member}", timeseries)
                    

#merged_ds = xr.concat([gefs_sel,cfsv2_sel,seas_sel], dim='time')













# old, not used now
import pandas as pd
import xarray as xr
import numpy as np
from datetime import datetime

# Step 1: Read in the stage data
def read_stage_data(file_path):
    df = pd.read_csv(file_path, parse_dates=['Date'])
    df = df.sort_values(by='Date')
    df['Stage'] = df['Stage'].apply(lambda x: float(x.replace(',', '')))
    df['Stage'] = df['Stage'].astype(float)
    df.set_index('Date', inplace=True)
    return df

# Step 2: Open the ERA5 and SEAS5 Zarr files
def open_zarr_files(era5_path, seas5_path):
    era5_ds = xr.open_zarr(era5_path, consolidated=True)
    seas5_ds = xr.open_zarr(seas5_path, consolidated=True)
    return era5_ds, seas5_ds

# Step 3: Read in the data for all selected variables for the ERA5 data
def read_era5_data(era5_ds, start_date, end_date, lat, lon, offsets, variables, feature_lags):
    era5_data = load_era5_data_v2(
        era5_ds, start_date, end_date, lat, lon, offsets, variables, feature_lags)
    return era5_data

# Step 4: Loop over each member of the SEAS5 data and process it
def process_seas5_data(seas5_ds, era5_data, start_date, end_date, lat, lon, offsets, variables, feature_lags):
    most_recent_init_time = seas5_ds.init_time.max().values
    lead_times = seas5_ds.lead_time.values

    combined_data_list = []

    for member in seas5_ds.member.values:
        seas5_member = seas5_ds.sel(member=member)
        seas5_data = load_seas5_data(
            seas5_member, most_recent_init_time, lead_times, lat, lon, variables, offsets)

        if np.isnan(seas5_data).any():
            continue

        seas5_df = pd.DataFrame(seas5_data,
                                index=pd.date_range(start=pd.Timestamp(most_recent_init_time),
                                                    periods=len(lead_times), freq='D'))

        combined_df = pd.concat([era5_data, seas5_df], axis=1)
        combined_df = combined_df.loc[start_date:end_date]

        combined_data_list.append(combined_df)

    return combined_data_list

# Step 5: Calculate the time-lagged streamflow stage data
def calculate_time_lagged_stage_data(stage_data, lookahead):
    lagged_data = create_lagged_feature(stage_data.values, lookahead)
    stage_data_lagged = pd.DataFrame(lagged_data, index=stage_data.index, columns=[f'Stage_lag_{i+1}' for i in range(lookahead)])
    return stage_data_lagged

# Step 6: Concatenate the stitched ERA5-SEAS5 member data with the streamflow stage data
def concatenate_data(combined_data_list, stage_data, lookahead):
    long_term_mean_stage = stage_data['Stage'].mean()
    
    for combined_df in combined_data_list:
        combined_df['Stage'] = stage_data['Stage']
        combined_df.fillna(long_term_mean_stage, inplace=True)
        
        time_lagged_stage_data = calculate_time_lagged_stage_data(stage_data, lookahead)
        combined_df = pd.concat([combined_df, time_lagged_stage_data], axis=1)
        
        combined_df.fillna(long_term_mean_stage, inplace=True)

    return combined_data_list

# run the entire workflow
stage_data_path = 'data/Mississippi River _at_ Arkansas City_ AR.csv'
era5_path = 's3://climate-ai-data-eng-prod-processed/era5/v1/daily.zarr/'
seas5_path = 'path_to_seas5_zarr_file.zarr'
lat = 33.5515
lon = -91.2384
offsets = [(0, 0), (3, 3), (3, -3.24), (1.5, 0)]
variables = ['t2m_max', 'tp', 'rh', 't2m_min']
feature_lags = [0, 0, 0, 0]
hacky_lookahead = 180

    # Step 1
stage_data = read_stage_data(stage_data_path)
start_date = stage_data.index.min()
end_date = stage_data.index.max()

    # Step 2
era5_ds, seas5_ds = open_zarr_files(era5_path, seas5_path)

    # Step 3
era5_data = read_era5_data(era5_ds, start_date, end_date, lats, lons, variables, feature_lags)
era5_df = pd.DataFrame(era5_data, index=pd.date_range(start=start_date, end=end_date, freq='D'))

    # Step 4
combined_data_list = process_seas5_data(seas5_ds, era5_df, start_date, end_date, lats, lons, variables, feature_lags)

    # Step 6
final_combined_data_list = concatenate_data(combined_data_list, stage_data, hacky_lookahead)

    # Print or save the final combined data
for i, combined_data in enumerate(final_combined_data_list):
    combined_data.to_csv(f'final_combined_data_member_{i}.csv')
    print(combined_data.head())
