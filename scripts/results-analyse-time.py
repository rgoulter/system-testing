#! /usr/bin/env python2

# I want to see how much could be tested, using a moderately-long amount of time.
# i.e. How much of an effect is due to TIMEOUTs, and other long-running tests.

from os import listdir, rename
from os.path import isfile, isdir, join
import sys
import json
import math

TIMEOUT = 300*1000

# Example of Results JSON (for valid test):
# {
#   "command" : "hip",
#   "results" : [ ... ],
#   "executionTime" : "515",
#   "filename" : "examples/working/hip/threads/no-deadlock-nonlexical.ss",
#   "arguments" : "--en-para --en-thrd-resource -tp parahip --en-lsmu-infer"
# }
#
# has "remarks" key if invalid.
def load_json_file(json_path):
	f = open(json_path)
	content = f.read()
	data = json.loads(content)
	f.close()

	# "purify" the data to a form we care about: (time, 
	time = int(data["executionTime"])

	if time > TIMEOUT:
		kind = "TIMEOUT"
	elif "remarks" in data:
		kind = "INVALID"
	else:
		kind = "VALID"

	# TODO: Could also consider whether the test "PASSED", "FAILED"

	return (time, kind)



timeof = lambda (time,kind): time
kindof = lambda (time,kind): kind



def filter_for_kind(kind):
	return lambda (xs) : filter(lambda (t,k): k == kind, xs)

filter_for_timeout = filter_for_kind("TIMEOUT")
filter_for_invalid = filter_for_kind("INVALID")
filter_for_valid = filter_for_kind("VALID")

def filter_for_time(time, xs):
	return filter(lambda (t,k): t <= time, xs)



def load_files_in_dir(dir_path):
	# Strictly assumes everything in dir_path is a json file.
	return [load_json_file(join(dir_path, res)) for res in listdir(dir_path)]



# def load_for_results_dir(results_path):
# 	# Strictly assumes everything in results_path is
# 	# a dir, (with a commit hash)..
# 	return [rename_files_in_dir(join(results_path, cmt)) for cmt in listdir(results_path)]



# So what to do with list of the results?
# For percentailes 10...90,
#  what the longest-running test is (i.e. value of the percentile),
#  summation of the percentiles i.e. how long to run to that percentile
#  percentage of all results captured by percentile;
#    percentage of passing, failing, invalid
def chart_results(ls_of_res):
	sorted_data = sorted(ls_of_res)

	sorted_valid_data = filter_for_valid(sorted_data)
	sorted_invalid_data = filter_for_invalid(sorted_data)

	def percentile_idx(p):
		return int(math.ceil(float(len(sorted_data) * p) / 100)) - 1

	def print_stats(xs, use_timeout = -1):
		# Longest Running
		longest = timeof(xs[-1])
		print "Longest Time:            %8d ms" % longest

		# Sum up till that point
		total = sum([timeof(d) for d in xs])
		print "Total Time (excl T/OUT): %8d ms" % total
		tout = max(use_timeout, longest) # use use_timeout, if given, otherwise use `longest`
		touts = total + (tout * (len(sorted_data) - len(xs)))
		print "Total Time (incl T/OUT): %8d ms" % touts


		# What % of all tests are "covered" by `xs`?
		print "Percent of All Tests:     %3d%%" % (100 * float(len(xs)) / len(sorted_data))
		# How many aren't?
		print "Complement size (T/OUT):  %d" % (len(sorted_data) - len(xs))

		print "Percent of VALID   tests: %3d%%" % (100 * float(len(filter_for_valid(xs))) / len(sorted_valid_data))
		print "Percent of INVALID tests: %3d%%" % (100 * float(len(filter_for_invalid(xs))) / len(sorted_invalid_data))



	print "Charting Results"
	print "(%d results)" % len(sorted_data)
	print

	# percentiles 10..90
	# n.b. ostensible < 80-percentile not great, so
	for percent in range(80, 100, 5):
		print "For %d Percentile:" % percent

		pidx = percentile_idx(percent)
		xs = sorted_data[:pidx+1]

		print_stats(xs)
		print

	# Want to investigate various proposed T/OUT times.
	# The largest not-TIMEOUT-time is ~138s, which would surely cost too much.
	# for time in range(1800, 3200, 200):
	for time in range(1500, 6000, 500):
		print "For tentative timeout %d ms:" % time

		xs = filter_for_time(time, sorted_data)

		print_stats(xs, time)
		print

	# For reference, if we do *all* tests:
	print "For All Tests:"
	print_stats(sorted_data)



if __name__ == "__main__":
	dirname = sys.argv[1]

	res = load_files_in_dir(dirname)

	chart_results(res)
	print

	# Find longest time, before timeout.
	# i.e. if time longer than this, 100% chance it will time out.
	# (if this is >> 2s, then will be expensive to run all tests..)
	print "Count > 300s: %d" % len(filter_for_timeout(res))
