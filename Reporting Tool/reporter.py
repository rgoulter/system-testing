import os
import time
import sys
import traceback
import subprocess
from utilities import Utilities
from hg_api import HgApi
from commit import Commit
import yaml # pip install pyyaml

class Reporter():

	hg = None
	settings = None
	utilities = None

	def __init__(self):
		global hg
		global settings
		global utilities
		utilities = Utilities()
		hg = HgApi()
		settings = yaml.load(open('settings.yaml').read())

	def setup(self):
		utilities.ensure_output_directory_exists(settings['app']['output_directory_location'], settings['app']['output_directory_name'])
		utilities.set_directory(settings['repository']['loris_local'])

	def process_branch(self, branch_name):
		try:
			hg.checkout_branch(branch_name)
			time_period = settings['app']['time_period']
			if utilities.check_last_commit_date(hg.last_commit_date(branch_name), time_period):
				print("Running tests on %s" % branch_name)
				utilities.create_directory(settings['app']['output_directory_location'] + settings['app']['output_directory_name'] + '/' + branch_name)
				# Checkout each rev, create directory accordingly and run
				commit_list = hg.get_commit_list()
				for commit in commit_list:
					if utilities.check_last_commit_date(commit.date, time_period):
						self.run_all_tests(branch_name, commit)
				print("Done with commits branch %s" % branch_name)
		except:
			print("Error processing branch %s" % branch_name)
			print traceback.format_exc()

	def run_all_tests(self, branch_name, commit):
	    for test_number in range(0, len(settings['tests'])):
	        utilities.create_directory(settings['app']['output_directory_location'] + settings['app']['output_directory_name'] + '/' + branch_name + '/' + settings['tests'][test_number]['name'])
	        self.run_test(commit, branch_name, test_number)

	def run_tests_on_commit(self, branch_name, commit_hash, test_number):
	    hg.checkout_commit(commit_hash)
	    self.run_test(commit_hash, branch_name, test_number)

	"""
		Returns output directory name of the format: branch_name/test_name
	"""
	def get_output_directory(self, branch_name, test_number):
	    return settings['app']['output_directory_location'] + settings['app']['output_directory_name'] + '/' + branch_name + '/' + settings['tests'][test_number]['name'] + '/'

	"""
		Returns file name of the format revision_commit_date
	"""
	def get_output_file_name(self, directory_name, commit, test_number):
		return  directory_name + commit.rev_id + '_' + utilities.convert_timestamp_to_string(commit.date) + settings['tests'][test_number]['file_extension']

	def run_test(self, commit, branch_name, test_number):
	    cwd = os.getcwd()
	    print("Currently in: " + cwd)
	    os.chdir(settings['tests'][test_number]['directory'])
	    directory_name = self.get_output_directory(branch_name, test_number)
	    output_file_name = self.get_output_file_name(directory_name, commit, test_number)
	    if not os.path.isfile(output_file_name):
	        output = open(output_file_name, 'w+')
	        print("Running Test on commit %s" % commit.hash)
	        handle = subprocess.call([settings['tests'][test_number]['command']], shell = True, stdout = output)
	        print("Output stored in directory %s/%s" % (commit.hash, output_file_name))
	    os.chdir(cwd)

	"""
		This method is called to start running all the tests on the given repository (settings.yaml)
	"""
	def run(self):
		self.setup()
		hg.pull()
		branches = hg.list_all_branches().split("\n")
		for branch in branches:
			branch_name = branch.split(" ")[0]
			self.process_branch(branch_name)