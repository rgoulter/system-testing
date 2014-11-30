INPUT_FILE_NAME = "text_input"
OUTPUT_FILE_NAME = "DSL.scala"
KEY_IDENTIFIER = "=>"
OUTPUT_DIRECTORY = "results"
INPUT_DIRECTORY = "/home/rohit/hg/sleek_hip/examples/working/sleek/"
COMMENT = "#"

''' 
Examples:

Input
-----

["label-basic.slk", "--dis-eps", (), "Fail.Valid.Valid.Fail."],

DSL output:
----------
new SleekTestCaseBuilder runCommand "sleek" onFile 
"/home/rohit/hg/sleek_hip/examples/working/sleek/sleek9.slk" 
withArguments "--elp" storeOutputInDirectory "results" withOutputFileName "sleek9.out" checkAgainst "Valid, Fail, Valid, Valid"

'''

def read_script():
	global script
	with open(INPUT_FILE_NAME) as f:
		script = f.readlines()

def process_script():
	for line in script:
		process_line(line)

def sanitise_input(word):
	return word.strip("[").strip("]").replace('"', '')

def sanitise_file_name(file_name):
	return file_name.replace("/", "_").replace("-","_")

def wrap_quotes(word):
	return '"' + word + '"'

def parse_expected_output(expected_output):
	expected_output = expected_output.replace(".", ", ")
	return expected_output[0:len(expected_output) - 2].lstrip().rstrip()

def construct_test(file_name, arguments, expected_output):
	output_file = sanitise_file_name(file_name).replace(".slk", ".out")
	test_name = sanitise_file_name(file_name).replace(".slk", "Test")
	print("val " + test_name + " = ")
	print('new SleekTestCaseBuilder runCommand "sleek" onFile ' + wrap_quotes(INPUT_DIRECTORY + file_name) + ' withArguments ' + wrap_quotes(arguments) + ' storeOutputInDirectory ' + wrap_quotes(OUTPUT_DIRECTORY) + ' withOutputFileName ' + wrap_quotes(output_file) + ' checkAgainst ' + wrap_quotes(expected_output))
	print(test_name + ".build " + "generateOutput()")
	print('\n')

def process_line(line):
	line = line.strip()
	if(KEY_IDENTIFIER not in line and COMMENT not in line and len(line) > 2):
		components = line.split(',')
		file_name = sanitise_input(components[0])
		arguments = sanitise_input(components[1])
		expected_output = parse_expected_output(sanitise_input(components[3]))
		construct_test(file_name, arguments, expected_output)

read_script()
process_script()