import os
import datetime

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

def check_last_commit_date(commit_date):
	current_milli_time = lambda: int(time.time())
	return (current_milli_time() - int(commit_date) <= (settings['app']['time_period'] * ONE_DAY))