import pandas as pd
import numpy as np

# Load the dataset
file_path = '/Users/chrisoldnall/Library/Mobile Documents/com~apple~CloudDocs/Downloads/HealthBoard_2019_Populationestimates.csv'
data = pd.read_csv(file_path)

# Filter out aggregated data based on 'HBQF' and 'SexQF' columns and exclude health board 'S92000003'
filtered_data = data[(data['HB'] != 'S92000003') & (data['HBQF'] != 'd') & (data['SexQF'] != 'd')]

# Identify age columns, excluding 'AllAges'
age_columns = [col for col in filtered_data.columns if 'Age' in col and col != 'AllAges']

# Initialize empty DataFrames to store the estimated populations for 2022 and 2023
estimated_populations = pd.DataFrame()

# Process each health board and calculate estimates
for hb, group in filtered_data.groupby(['HB']):
    for year in [2022, 2023]:
        for sex in ['Male', 'Female']:
            # Get the last 10 years of data for calculating growth rates
            latest_data = group[(group['Sex'] == sex)].sort_values('Year').tail(10)
            # Calculate the average annual growth rate for each age column
            growth_rates = (latest_data[age_columns].iloc[-1] / latest_data[age_columns].iloc[0]) ** (1/9) - 1
            # Estimate the population for the year using the latest year's data and the calculated growth rate
            last_year_population = latest_data[age_columns].iloc[-1]
            estimated_population = last_year_population * (1 + growth_rates)
            # Prepare and append the estimated row, rounded to whole numbers
            estimated_row = {
                'HB': hb,
                'Year': year,
                'HBQF': 'ce',
                'Sex': sex,
                'SexQF': 'ce',
                **{col: np.round(estimated_population[col], 0) for col in age_columns}
            }
            estimated_populations = estimated_populations.append(estimated_row, ignore_index=True)

# Calculate 'All' estimates for each health board and year by aggregating Male and Female estimates
for hb, year in estimated_populations[['HB', 'Year']].drop_duplicates().values:
    male_estimates = estimated_populations[(estimated_populations['HB'] == hb) & (estimated_populations['Year'] == year) & (estimated_populations['Sex'] == 'Male')]
    female_estimates = estimated_populations[(estimated_populations['HB'] == hb) & (estimated_populations['Year'] == year) & (estimated_populations['Sex'] == 'Female')]
    all_estimates = male_estimates[age_columns].values + female_estimates[age_columns].values
    all_row = {
        'HB': hb,
        'Year': year,
        'HBQF': 'ce',
        'Sex': 'All',
        'SexQF': 'ce',
        **{col: np.round(all_estimates[0][i], 0) for i, col in enumerate(age_columns)}
    }
    estimated_populations = estimated_populations.append(all_row, ignore_index=True)

# Add the 'AllAges' column as the sum of all age-specific estimates and round to whole numbers
estimated_populations['AllAges'] = estimated_populations[age_columns].sum(axis=1).round(0)

# Assign an _id that continues the order from the original data
max_id = data['_id'].max()
estimated_populations['_id'] = range(max_id + 1, max_id + 1 + len(estimated_populations))

# Append the estimated populations to the original DataFrame, maintaining the year and HB order
combined_data = pd.concat([data, estimated_populations], ignore_index=True, sort=False)
combined_data = combined_data.sort_values(by=['Year', 'HB', '_id']).reset_index(drop=True)

# Verify the final DataFrame
combined_data.tail()


combined_data.to_csv("/Users/chrisoldnall/Library/Mobile Documents/com~apple~CloudDocs/Downloads/UpdatesPopulationEstimates.csv")