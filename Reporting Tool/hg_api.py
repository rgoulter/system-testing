import os
import time
import settings
import subprocess
import hg_commands as commands

def setup():
	ensure_output_directory_exists()
	set_directory()

def ensure_output_directory_exists():
	directory = settings.app.output_directory_location + settings.app.output_directory_name
	if not os.path.exists(directory):
	    os.makedirs(directory)

def set_directory():
	os.chdir(settings.repository['loris_local'])

def execute(call_string):
	handle = os.popen(call_string)
	return handle.read()

def list_all_branches():
	return execute(commands.list_all_branches)

def pull():
	execute(commands.pull)

def checkout_branch(branch_name):
	execute(commands.update + " " + branch_name)

def process_branch(branch_name):
	try:
		checkout_branch(branch_name)
		if check_last_commit_date():
			print("Running tests on %s" % branch_name)
			run_test()
	except:
		print("Error processing branch %s" % branch_name)

def last_commit_date():
	return execute(commands.last_commit_date).split()[0]

def check_last_commit_date():
	current_milli_time = lambda: int(time.time())
	return (current_milli_time() - int(last_commit_date()) <= settings.app['time_period'])

def run_test():
	p = subprocess.Popen([settings.test.command], cwd = settings.test.directory)
	p.wait()

def run():
	setup()
	pull()
	branches = list_all_branches().split("\n")
	for branch in branches:
		branch_name = branch.split(" ")[0]
		# print("Checking branch: %s" % branch_name)
		process_branch(branch_name)
run()