import os
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

set_directory()
print(list_all_branches())
pull()
