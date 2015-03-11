INPUT_FILE_NAME = "hip_input.pl"
OUTPUT_FILE_NAME = "Hip.scala"
KEY_IDENTIFIER = "=>"
OUTPUT_DIRECTORY = "results"
INPUT_DIRECTORY = "/home/rohit/hg/sleek_hip/examples/working/hip/"
COMMENT = "#"
USAGE_TEMPLATE = "Hip.template"
NEW_LINE = '\n'

''' 
Examples:

Input
-----

"array"=>[
		["arr_at.java",1,"","main","SUCCESS"],
		["arr_binarysearch.java",1,"","binary_search","SUCCESS"],
		["arr_search_decrease_less_than_two.java",1,"","searchzero","FAIL"], # induction required
		["arr_bubblesort.java",2,"","bubblesort","SUCCESS","bubble","SUCCESS"],
		["arr_bubblesort_perm.java",2,"","bubblesort","SUCCESS","bubble","SUCCESS"],
		["arr_double.java",1,"","doublearr","SUCCESS"],
		["arr_extract_nonzeros.java",3,"","copy_nonzeros","SUCCESS","count_nonzeros","SUCCESS","extract_nonzeros","SUCCESS"],
		["arr_init.java",1,"","zinit","SUCCESS"],
		["arr_insertsort.java",2,"","insertelm","SUCCESS","insertion_sort","SUCCESS"],
		["arr_insertsort_perm.java",2,"","insertelm","SUCCESS","insertion_sort","SUCCESS"],
		["arr_invert.java",2,"","Invert","SUCCESS","InvertHelper","SUCCESS"],
		["arr_max.java",1,"","max_value_of_array","SUCCESS"],
		["arr_mergesort.java",3,"","merge_sorted_arrays","SUCCESS","copy_array","SUCCESS","merge_sort","SUCCESS"],
		["arr_new_exp.java",1,"","main","SUCCESS"],
		["arr_nqueens.java",3,"","nQueens","SUCCESS","nQueensHelper","SUCCESS","nQueensHelperHelper","SUCCESS"],
		["arr_qsort.java",2,"","arraypart","SUCCESS","qsort","SUCCESS"],
		["arr_rev.java",1,"","arrayrev","SUCCESS"],
		["arr_selectionsort.java",2,"","array_index_of_max","SUCCESS","selection_sort","SUCCESS"],
		["arr_selectionsort_perm.java",2,"","array_index_of_max","SUCCESS","selection_sort","SUCCESS"],
		["arr_sparse.java",3,"--imm","create","SUCCESS","get","SUCCESS","setsa","SUCCESS"],
		["arr_sum.java",2,"","sigmaright","SUCCESS","sigmaleft","SUCCESS"] # there is an axiom that requires induction
	],

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
	# Filter out comments
	sanitized_script = []
	for line in script:
		if "#" in line:
			comment = line[line.index("#"):]
			# print(comment)
			line = line.replace(comment, "")
		if len(line.strip()) > 0:
			sanitized_script.append(line.strip())
	script = sanitized_script

def sanitise_input(word):
	return word.strip("[").strip("]").replace('"', '')

def sanitise_file_name(file_name):
	return file_name.replace("/", "_").replace("-","_")

def wrap_quotes(word):
	return '"' + word + '"'

def construct_hip_test_in_suite(file_name, arguments, expected_output, current_test_directory):
	output_file = sanitise_file_name(file_name).replace(".java", ".out").replace(".ss", ".out").replace('"',"")
	arguments = arguments.replace('"', "")
	file_name = file_name.replace('"', "")
	builder_test = "suite addTest(" + '"hip"' + "," + wrap_quotes(INPUT_DIRECTORY + current_test_directory + "/" + file_name) + "," + wrap_quotes(arguments) + "," + wrap_quotes(OUTPUT_DIRECTORY) + "," + wrap_quotes(output_file) + "," + wrap_quotes(expected_output) + ")" + NEW_LINE
	# print(builder_test)
	return builder_test

def get_expected_output(elements):
	number_of_results = int(elements[1])
	expected_output = ""
	i = 3
	while i < 3 + (number_of_results * 2):
		if '"SUCCESS"' in elements[i]:
			i += 1
		else:
			expected_output  += elements[i] + ": " + elements[i+1] +", "
			i += 2
	expected_output = expected_output[:-2].replace("]", "").replace("[", "").replace('"', "")
	# print(expected_output)
	return expected_output

def process_line():
	current_test_directory = ''
	for line in script:
		line = line.strip()
		if KEY_IDENTIFIER in line:
			line = line.replace(" ","")
			identifier_index = line.index(KEY_IDENTIFIER)
			current_test_directory = line[1:identifier_index].replace('"',"").strip()
		else: #test
			line = line.replace("[", "").replace("],", "")
			elements = line.split(",")
			expected_output = get_expected_output(elements)
			case = construct_hip_test_in_suite(elements[0], elements[2], get_expected_output(elements), current_test_directory)
			print(case)

read_script()
process_line()
