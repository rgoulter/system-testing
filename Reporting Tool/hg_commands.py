list_all_branches = "hg branches -a"

pull = "hg pull"

update = "hg update -C"
	
last_commit_date = 'hg log -r tip -T "{date|hgdate}\n"'

log = "hg log"

whitespace = ' '

max_number = '-l50'

template_option = '-T'

commit_template = r'"{node}_{date|hgdate}\n"'

commit_list = log + whitespace + max_number + whitespace + template_option + whitespace + r'"{node}_{date|hgdate}\n"'