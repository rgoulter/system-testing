import os
import time
import settings
from hg_api import HgApi

def ensure_output_directory_exists():
	directory = settings.app['output_directory_location'] + settings.app['output_directory_name']
	if not os.path.exists(directory):
	    os.makedirs(directory)

def set_directory():
	os.chdir(settings.repository['loris_local'])

def setup():
	global hg
	hg = HgApi()
	# ensure_output_directory_exists()
	set_directory()

def execute(call_string):
	handle = os.popen(call_string)
	return handle.read()

def process_branch(branch_name):
	try:
		hg.checkout_branch(branch_name)
		if check_last_commit_date():
			print("Running tests on %s" % branch_name)
			run_test(branch_name)
	except:
		print("Error processing branch %s" % branch_name)

def check_last_commit_date():
	current_milli_time = lambda: int(time.time())
	return (current_milli_time() - int(hg.last_commit_date()) <= settings.app['time_period'])

def get_output_file_name(branch_name):
	return settings.test['output_prefix'] + branch_name + settings.test['output_file_extension']

def run_test(branch_name):
	cwd = os.getcwd()
	print("Currently in: " + cwd)
	os.chdir(settings.test['directory'])
	output_file_name = get_output_file_name(branch_name)
	output = open(output_file_name, 'w+')
	print("Running Test on branch %s" % branch_name)
	handle = subprocess.call([settings.test['command']], shell = True, stdout = output)
	print("Output stored in file %s" % output_file_name)
	os.chdir(cwd)

def run():
	setup()
	hg.pull()
	branches = hg.list_all_branches().split("\n")
	for branch in branches:
		branch_name = branch.split(" ")[0]
		process_branch(branch_name)

run()