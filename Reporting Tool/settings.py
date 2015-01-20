# Seconds for each unit of time, use inside app['time_period']
ONE_WEEK = 604800000
ONE_MONTH = 4 * ONE_WEEK
ONE_YEAR = 12 * ONE_MONTH

vcs = dict(
	client = "hg",
	branches = "all"
)

# Remote repo URL
repository = dict(
	remote = "",
	loris_local = "/home/rohit/hg/sleek_hip/",
	local = "/Users/rohitmukherjee/dev/repositories/scalaWorkspace/High-Performance-DSLs/Reporting Tool/test_repo"
)

# App settings
app = dict(
	# Time period to run tests of last commit
	time_period = 3 * ONE_MONTH, # Run tests for commits made in the last months
	output_directory_name = "Sleek_Test_Results",
	# Must end with a '/'
	output_directory_location = "/home/rohit/High-Performance-DSLs/Reporting Tool/"
)

# Test settings
test = dict(
	command = 'sbt run',
	directory = '/home/rohit/High-Performance-DSLs/',
	output_file_extension = '.out',
	output_prefix = 'sleek_test_')