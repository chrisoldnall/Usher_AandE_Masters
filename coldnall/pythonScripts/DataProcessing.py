import pandas as pd 
import numpy as np 
pd.set_option('display.max_columns', None)
import matplotlib.pyplot as plt

weekAttendance = pd.read_csv("/Users/chrisoldnall/Library/Mobile Documents/com~apple~CloudDocs/PhD/GitHub Repositories [PROPER]/Usher_AandE_Masters/data/11_09_23_Download/2023-09-05-ed-weekly-attendance-and-waiting-times-data.csv")
weekAttendanceColumns = list(weekAttendance.columns)
numberOfRecords = len(list(set(weekAttendance["WeekEndingDate"].tolist()))) # 445 weeks (from 2015 to 2023)
numberOfLocations = len(list(set(weekAttendance["LocationCode"].tolist()))) # 32 Hospitals
HospitalCodeNames = weekAttendance[["LocationCode", "LocationName"]].drop_duplicates(subset=['LocationCode', 'LocationName'])

# Using S314H (RI Edinburgh) from now on for testing purposes.
RIAttendance = weekAttendance[weekAttendance['LocationCode'] == "S314H"]
RIAttendance['WeekEndingDate'] = pd.to_datetime(RIAttendance['WeekEndingDate'])
plt.plot(RIAttendance['WeekEndingDate'], RIAttendance['PercentageWithin4HoursEpisode'])
plt.locator_params(axis='x', nbins=1)
plt.title("Success Rate (Within 4 Hours) for:\n"+ r"$\bf{" + "The \ Royal \ Infirmary \ Edinburgh \ A&E." + "}$")
plt.xlabel("Week")
plt.ylabel("Success Percentage\n(Number of Attendances / Number Seen Within 4 Hours)")
plt.savefig("/Users/chrisoldnall/Library/Mobile Documents/com~apple~CloudDocs/PhD/GitHub Repositories [PROPER]/Usher_AandE_Masters/coldnall/results/RIEdinburgh/SuccessRate.png")
plt.close()
plt.plot(RIAttendance['WeekEndingDate'], RIAttendance['PercentageOver12HoursEpisode'], color="red")
plt.title("Failure Rate (Over 12 Hours) for:\n"+ r"$\bf{" + "The \ Royal \ Infirmary \ Edinburgh \ A&E." + "}$")
plt.xlabel("Week")
plt.ylabel("Failure Percentage\n(Number of Attendances / Number Not Seen Within 12 Hours)")
plt.savefig("/Users/chrisoldnall/Library/Mobile Documents/com~apple~CloudDocs/PhD/GitHub Repositories [PROPER]/Usher_AandE_Masters/coldnall/results/RIEdinburgh/FailureRate.png")
plt.close()

# Lorn & Islands Hospital
RIAttendance = weekAttendance[weekAttendance['LocationCode'] == "C121H"]
RIAttendance['WeekEndingDate'] = pd.to_datetime(RIAttendance['WeekEndingDate'])
plt.plot(RIAttendance['WeekEndingDate'], RIAttendance['PercentageWithin4HoursEpisode'])
plt.locator_params(axis='x', nbins=1)
plt.title("Success Rate (Within 4 Hours) for:\n"+ r"$\bf{" + "Lorn \ and \ Islands \ Oban \ A&E." + "}$")
plt.xlabel("Week")
plt.ylabel("Success Percentage\n(Number of Attendances / Number Seen Within 4 Hours)")
plt.savefig("/Users/chrisoldnall/Library/Mobile Documents/com~apple~CloudDocs/PhD/GitHub Repositories [PROPER]/Usher_AandE_Masters/coldnall/results/LANDI/SuccessRate.png")
plt.close()
plt.plot(RIAttendance['WeekEndingDate'], RIAttendance['PercentageOver12HoursEpisode'], color="red")
plt.title("Failure Rate (Over 12 Hours) for:\n"+ r"$\bf{" + "Lorn \ and \ Islands \ Oban \ A&E." + "}$")
plt.xlabel("Week")
plt.ylabel("Failure Percentage\n(Number of Attendances / Number Not Seen Within 12 Hours)")
plt.savefig("/Users/chrisoldnall/Library/Mobile Documents/com~apple~CloudDocs/PhD/GitHub Repositories [PROPER]/Usher_AandE_Masters/coldnall/results/LANDI/FailureRate.png")
plt.close()