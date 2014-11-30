INPUT_FILE_NAME = "text_input"
OUTPUT_FILE_NAME = "DSL.scala"
KEY_IDENTIFIER = "=>"
OUTPUT_DIRECTORY = "results"
INPUT_DIRECTORY = "/home/rohit/hg/sleek_hip/examples/working/sleek/"
COMMENT = "#"

# ["label-basic.slk", "--dis-eps", (), "Fail.Valid.Valid.Fail."],
#new SleekTestCaseBuilder runCommand "sleek" onFile 
# "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek9.slk" 
# withArguments "--elp" storeOutputInDirectory "results" withOutputFileName "sleek9.out" checkAgainst "Valid, Fail, Valid, Valid"

def read_script():
	global script
	with open(INPUT_FILE_NAME) as f:
		script = f.readlines()

def process_script():
	for line in script:
		process_line(line)

def sanitise_word(word):
	return word.strip("[").strip("]").replace('"', '')

def wrap_quotes(word):
	return '"' + word + '"'

def parse_expected_output(expected_output):
	expected_output = expected_output.replace(".", ", ")
	return expected_output[0:len(expected_output) - 2].lstrip().rstrip()

def process_line(line):
	line = line.strip()
	if(KEY_IDENTIFIER not in line and COMMENT not in line and len(line) > 2):
		components = line.split(',')
		file_name = sanitise_word(components[0])
		arguments = sanitise_word(components[1])
		output_file = file_name.rstrip(".slk") + ".out"
		expected_output = parse_expected_output(sanitise_word(components[3]))
		# print(file_name)
		# print(arguments)
		# print(output_file)
		# print(expected_output)
		print('new SleekTestCaseBuilder runCommand "sleek" onFile ' + wrap_quotes(INPUT_DIRECTORY + '/' + file_name) + ' withArguments ' + wrap_quotes(arguments) + ' storeOutputInDirectory ' + wrap_quotes(OUTPUT_DIRECTORY) + ' withOutputFileName ' + wrap_quotes(output_file) + ' checkAgainst ' + wrap_quotes(expected_output))
		print('\n')
		 
read_script()
process_script()