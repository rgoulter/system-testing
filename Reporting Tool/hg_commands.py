class HgCommands():

	''' Encapsulates string commands for mercurial command line API'''

	whitespace = ' '

	max_number = '-l50'

	template_option = '-T'

	def __init__(self):
		pass

	def list_all_branches(self):
		return "hg branches -a"

	def pull(self):
		return "hg pull"

	def update(self):
		return "hg update -C"

	def checkout_commit(self):
		return 'hg update -c' # Takes one parameter <cset>

	def last_commit_date(self):
		return 'hg log -l 1 -T "{date|hgdate}\n" --only-branch' # Takes one parameter <branch_name>

	def log(self):
		return "hg log"

	def revert(self):
		return "hg revert -C -r "

	def commit_template(self):
		return r'"{node}_{date|hgdate}\n"'

	def local_commit_template(self):
		return r'"{rev}_{date|hgdate}\n"'

	def commit_list(self):
		return (self.log() + self.whitespace + self.max_number + self.whitespace + self.template_option + self.whitespace + r'"{rev}_{node}_{date|hgdate}\n"')