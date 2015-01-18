import os
import time
import settings
import hg_commands as commands

def set_directory():
	os.chdir(settings.repository['local'])

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
	except:
		print("Error processing branch %s" % branch_name)

def last_commit_date():
	return execute(commands.last_commit_date).split()[0]

def check_last_commit_date():
	current_milli_time = lambda: int(time.time())
	# print(current_milli_time())
	# print(int(last_commit_date()))
	return (current_milli_time() - int(last_commit_date()) <= settings.app['time_period'])

def run():
	set_directory()
	branches = list_all_branches().split("\n")
	pull()
	for branch in branches:
		branch_name = branch.split(" ")[0]
		print("Checking branch: %s" % branch_name)
		process_branch(branch_name)
run()