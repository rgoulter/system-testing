import os

def execute(call_string):
	handle = os.popen(call_string)
	return handle.read()

def create_directory(directory):
	if not os.path.exists(directory):
         os.makedirs(directory)

def set_directory(directory_name):
	os.chdir(directory_name)
