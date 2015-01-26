import os
import time
import hg_commands as commands

class HgApi:
	'''
		Wraps the mercurial commands in a convenient api 
	'''
	
	def __init__(self):
		pass

	def _execute(self, call_string):
		handle = os.popen(call_string)
		return handle.read()

	def list_all_branches(self):
		return self._execute(commands.list_all_branches)

	# Executes an 'hg pull' in cwd
	def pull(self):
		self._execute(commands.pull)

	def checkout_branch(self, branch_name):
		self._execute(commands.update + " " + branch_name)

	def last_commit_date(self):
		return self._execute(commands.last_commit_date).split()[0]

	def revert(self):
		self._execute(commands.revert)

	def get_commit_list(self):
		commit_list = self._execute(commands.commit_list).split("\n")
		commits = []
		for commit in commit_list:
			commit_info = commit.split("_")
			commits.append((commit_info[0], commit_info[-1]))
		return commits