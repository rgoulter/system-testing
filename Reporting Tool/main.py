import os
import time
import settings
import sys
import traceback
import subprocess
import yaml # pip install pyyaml
from hg_api import HgApi

ONE_DAY = 24 * 60 * 60

def ensure_output_directory_exists():
	directory = settings['app']['output_directory_location'] + settings['app']['output_directory_name']
	if not os.path.exists(directory):
	    os.makedirs(directory)

def set_directory():
	os.chdir(settings['repository']['loris_local'])

def setup():
	global hg
	global settings
	settings = yaml.load(open('settings.yaml').read())
	hg = HgApi()
	ensure_output_directory_exists()
	set_directory()

def execute(call_string):
	handle = os.popen(call_string)
	return handle.read()

def process_branch(branch_name):
	try:
		hg.checkout_branch(branch_name)
		if check_last_commit_date(hg.last_commit_date(branch_name)):
			print("Running tests on %s" % branch_name)
			# Checkout each rev, create directory accordingly and run
			commit_list = hg.get_commit_list()
			for commit in commit_list:
				if check_last_commit_date(commit[1]):
					print(commit[0])
			print("Done with commits branch %s" % branch_name)
	except:
		print("Error processing branch %s" % branch_name)
		print traceback.format_exc()

def check_last_commit_date(commit_date):
	current_milli_time = lambda: int(time.time())
	return (current_milli_time() - int(commit_date) <= (settings['app']['time_period'] * ONE_DAY))

def get_output_file_name(branch_name):
	return settings['test']['prefix'] + branch_name + settings['test']['file_extension']

def run_test(branch_name):
	cwd = os.getcwd()
	print("Currently in: " + cwd)
	os.chdir(settings['test']['directory'])
	output_file_name = get_output_file_name(branch_name)
	output = open(settings['app']['output_directory_location'] + settings['app']['output_directory_name'] + '/' +  output_file_name, 'w+')
	print("Running Test on branch %s" % branch_name)
	handle = subprocess.call([settings['test']['command']], shell = True, stdout = output)
	print("Output stored in file %s" % output_file_name)
	os.chdir(cwd)

def run():
	setup()
	hg.pull()
	branches = hg.list_all_branches().split("\n")
	print(branches)
	for branch in branches:
		branch_name = branch.split(" ")[0]
		process_branch(branch_name)

run()
# setup()
# print(hg.get_commit_list())
# print(check_last_commit_date(1413039744))
# print(check_last_commit_date(1413039748))
