import os
import time
import hg_commands as commands
from commit import Commit

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
		return self._execute(commands.list_all_branches)[:-1]

	# Executes an 'hg pull' in cwd
	def pull(self):
		self._execute(commands.pull)

	def checkout_branch(self, branch_name):
		self._execute(commands.update + " " + branch_name)

	def checkout_commit(self, commit_hash):
		self._execute(commands.checkout_commit + ' ' + commit_hash)

	def last_commit_date(self, branch_name):
		return self._execute(commands.last_commit_date + " " + branch_name).split()[0]

	def revert(self):
		self._execute(commands.revert)

	"""
		Returns a list of Commit objects (see commit.py) with attributes (rev_id, commit_hash, commit_date)
	"""
	def get_commit_list(self):
		commit_list = self._execute(commands.commit_list).split("\n")[:-1]
		commits = []
		for commit in commit_list:
			commit_info = commit.split("_")
			new_commit = Commit(commit_info[0], commit_info[1], commit_info[-1].split("-")[0])
			commits.append(new_commit)
		return commits
