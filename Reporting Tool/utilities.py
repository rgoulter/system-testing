import os
import datetime
import time

class Utilities():

	''' Encapsulates the utility methods used by other parts of the tool '''

	ONE_DAY = 24 * 60 * 60
	def __init__(self):
		pass

	def execute(self, call_string):
		handle = os.popen(call_string)
		return handle.read()

	def create_directory(self, directory):
		if not os.path.exists(directory):
	         os.makedirs(directory)

	def set_directory(self, directory_name):
		os.chdir(directory_name)

	def convert_timestamp_to_string(self, timestamp):
		return datetime.datetime.fromtimestamp(int(timestamp)).strftime('%Y-%m-%d %H:%M:%S')

	def check_last_commit_date(self, commit_date, time_period):
		current_milli_time = lambda: int(time.time())
		return (current_milli_time() - int(commit_date) <= time_period * self.ONE_DAY)

	def ensure_output_directory_exists(self, directory_location, directory_name):
		self.create_directory(directory_location + directory_name)
