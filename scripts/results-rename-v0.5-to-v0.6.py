#! /usr/bin/env python2

# Quick script to handle renaming of the result JSON files.

from os import listdir, rename
from os.path import isfile, isdir, join
import json


def update_json_file(json_path, hipOrSleek):
	f = open(json_path)
	content = f.read()
	data = json.loads(content)
	f.close()

	# Change "filename" key.
	data["filename"] = "examples/working/" + hipOrSleek + "/" + data["filename"]
	new_json = json.dumps(data)

	print "Writing updated JSON to ", json_path
	f = open(json_path, "w")
	f.write(new_json)
	f.close()


def rename_file(dir_path, json_filename):
	# json_path basename should be in format like..
	#   hip_threadsmotivexample2ss_enparaenthrdresourcetpredlog.json
	#   hip_termbenchsaprovejulia10recursiveevenoddss_.json
	[hipOrSleek, fn, args] = json_filename.split("_")

	new_filename = "_".join([hipOrSleek, "examplesworking" + hipOrSleek + fn, args])

	# if new_name exists: rm old (reduntant?)
	# else, mv old to new path
	f1 = join(dir_path, json_filename)
	f2 = join(dir_path, new_filename)

	print "renaming", f1, " to ", f2
	rename(f1, f2)


def rename_files_in_dir(dir_path):
	# Strictly assumes everything in dir_path is a json file.
	res_files = listdir(dir_path)
	for res in res_files:
		rename_file(dir_path, res)

		update_json_file(join(dir_path, res), res.split("_")[0])


def rename_for_results_dir(results_path):
	# Strictly assumes everything in results_path is
	# a dir, (with a commit hash)..
	commits = listdir(results_path)

	for cmt in commits:
		cmt_path = join(results_path, cmt)
		rename_files_in_dir(cmt_path)


if __name__ == "__main__":
	rename_for_results_dir("results")
