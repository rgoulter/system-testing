import os
import datetime
import time

ONE_DAY = 24 * 60 * 60

def execute(call_string):
	handle = os.popen(call_string)
	return handle.read()

def create_directory(directory):
	if not os.path.exists(directory):
         os.makedirs(directory)

def set_directory(directory_name):
	os.chdir(directory_name)

def convert_timestamp_to_string(timestamp):
	return datetime.datetime.fromtimestamp(int(timestamp)).strftime('%Y-%m-%d %H:%M:%S')

def check_last_commit_date(commit_date, time_period):
	current_milli_time = lambda: int(time.time())
	return (current_milli_time() - int(commit_date) <= time_period * ONE_DAY)

def ensure_output_directory_exists(directory_location, directory_name):
	create_directory(directory_location + directory_name)