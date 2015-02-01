list_all_branches = "hg branches -a"

pull = "hg pull"

update = "hg update -C"

checkout_commit = 'hg update -c' # Takes one parameter <cset>

last_commit_date = 'hg log -l 1 -T "{date|hgdate}\n" --only-branch' # Takes one parameter <branch_name>

log = "hg log"

revert = "hg revert -C -r "

whitespace = ' '

max_number = '-l50'

template_option = '-T'

commit_template = r'"{node}_{date|hgdate}\n"'

commit_list = log + whitespace + max_number + whitespace + template_option + whitespace + r'"{node}_{date|hgdate}\n"'
