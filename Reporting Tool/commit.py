class Commit:
	'Class to model Commit retrieved from Mercurial'
	rev_id = ''
	hash = ''
	date = ''

	def __init__(self, rev_id, hash, date):
		self.rev_id = rev_id
		self.hash = hash
		self.date = date

	def __str__(self):
		return self.hash + "_" + self.rev_id + "_" + self.date