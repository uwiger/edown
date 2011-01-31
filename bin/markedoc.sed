# markedoc 0.1 - 01/31/11 - H. Diedrich <hd2010@eonblast.com>
# ----------------------------------------------------------
# sed command file to convert markdown format to edoc format
# ----------------------------------------------------------
# Use it to make a markdown readme file part of an edoc file:
# sed -E -f <this file> <markdown file> > <edoc file>
# ----------------------------------------------------------
# SAMPLE USE:
# sed -E -f markedoc.sed README.markdown > overview.edoc
# ----------------------------------------------------------
# SAMPLE FILES:
# https://github.com/hdiedrich/markedoc/tree/master/samples
# ----------------------------------------------------------
# SAMPLE WORKFLOW:
# echo '@doc ' > samples/doc/SAMPLE.edoc
# sed -E -f bin/markedoc.sed samples/SAMPLE1.md >> samples/doc/SAMPLE.edoc
# erl -noshell -run edoc_run application "'myapp'" '"samples"' '[]' 
# ----------------------------------------------------------
# REQUIREMENTS: sed, Erlang
# ----------------------------------------------------------
# STATUS: Alpha. 
# You can do nice things but it likes to trip up EDoc.
# With a bit of patience, and mostly with pretty clean md
# markup, and some blank lines sometimes, most things work.
# ----------------------------------------------------------
# LICENSE: Free software, no warranties.
# ----------------------------------------------------------
# On edown: http://www.erlang.org/doc/apps/edoc/ 
# On Markdown: http://daringfireball.net/projects/markdown/
# On Edoc: http://www.erlang.org/doc/apps/edoc/ 
# ----------------------------------------------------------
# There are  many ways to create formats that will make the
# EDoc creator tilt and the  errors it throws are not quite
# illuminating to the reader sometimes. Make an incremental
# approach and see what works. As you can see from the live
# sample, it's quite a lot that does work and some bits can
# be worked out. Please experiment and push your fixes.
# - Thanks!
# ----------------------------------------------------------
# Repository: https://github.com/hdiedrich/markedoc/
# ----------------------------------------------------------
# Issues: https://github.com/hdiedrich/markedoc/issues
# ----------------------------------------------------------
# * Underlined ("==="/"---") headlines currently don't work,
#   use the '#' variant instead  
# * **'[1]: ...'-style end note references need two spaces
#   at the end of the line**    
# * add two new lines at end of your markdown file to avoid
#   loosing the last line.  
# * Local anchor jumps fail  
# * robust alternates not tested for some time  
# * space before javascript links should go  
# * protect ampersands
# ----------------------------------------------------------

# **********************************************************
# SCRIPT
# **********************************************************
# this is a sed script for -E regex and limited scripting.
# s/<find>/<replace>/<flag>  is the basic sed regex replace
# command.  sed  normally works strictly line by line.  'N'
# is used to join lines. 't' is a conditional branch. 'mlc:'
# is a label. The order of replacement functions matters.
# See 'man sed' for more info. If you are a sed master, 
# your help making this better is appreciated.

# code sample blocks, trying to get them into one <pre> block
# -----------------------------------------------------------
# tabs are consumed for 'navigation'. sed is Turing complete.
# inserted space is needed by edocs.
# There are tabs in this pattern.
/^	/ {
	# do ...
	:mlc
		# append next line
		N
		# does thatline start with a tab, too?
		s/(\n)	(.*)$/\1 \2/g
		# while: ... yes, then repeat from :mlc
		t mlc
	# if no, <pre> block is complete, though one line too much, store this.
	h
	# Handle the <pre> block to be:
	# -----------------------------
	# cut off the last line, that doesn't belong, and insert newlines
	s/^	(.*)(\n)([^\n]*)$/\2 \1\2/
	# wrap all in the docs code tags ```...'''
	s/^(.*)$/```\1'''/
	# protect @ (for edoc related texts that explain @-tags). There is a tab in [].
	s/([ 	\"\'\`]+@)/\1@/g
	# send result to stdout  
	p
	# Now make sure that last line is not lost:
	# -----------------------------------------
	# get stored back
	g
	# this time discard all but the last line, which is processed further
	s/^	(.*)(\n)([^\n]*)$/\3/
} 

# robust alternate for code blocks: each tabbed line
# --------------------------------------------------
# If the above keeps being difficult, use this more robust 
# version. The main difference is simply that it will tag each 
# line separately. If you work out the right margins and 
# paddings for <pre> in your css file, that might give just as
# nice results as the above. There are tabs in this pattern.
# s/^	(.+)$/```	\1'''/

# Erlang comments
# ---------------
# doesn't work yet 
# s/(\n\s*)(%[^\n]+)/\1<span class="comment">\2<\/span>/g

# links
# -----
# external links
s/\[([^]]+)\]\(([^)]+)\)/<a href=\"\2\">\1<\/a>/

# references - must have trailing double space! (could learn to look at next line for "...")
s/(\[([^]]+)\]): +\[?(http[s]?:\/\/[^.>" ]+\.[^>" ]+)\]? *	*("([^"]+)")? *	*$/\1 <a name="\2" id="\2" href="\3" target="_parent">\3<\/a> \5<br \/>/ 
s/(\[([^]]+)\]): +<?([^@>" ]+@[^.>" ]+\.[^>" ]+)>? *	*("([^"]+)")? *	*$/\1 <a name="\2" id="\2" href="mailto:\3">\3<\/a>\5<br \/>/ 

# smart reference for the [x]: ... format, jumping right to the referenced page.
# ------------------------------------------------------------------------------
s/\[([^]]+)\]\[\]/<a href="javascript: parent.document.location.href=document.getElementById('\1').href">\1<\/a>/g
s/\[([^]]+)\]\[([^]]+)\]/<a href="javascript: parent.document.location.href=document.getElementById('\2').href">\1<\/a>/g

# robust alternate reference for the [x]: ... format, jumping to footnote.
# ------------------------------------------------------------------------
# If you don't like the javascript tags, comment out the previous 'smart' 
# reference patterns and uncomment these.
# s/\[([^]]+)\]\[\]/<a href="#\1">\1<\/a>/g
# s/\[([^]]+)\]\[([^]]+)\]/<a href="#\2">\1<\/a>/g

# headlines by #
# --------------
# h1 demoted to h2 as h1 is reserved in edoc
s/^####(.+)$/====\1 ====/
s/^###(.+)$/===\1 ===/
s/^##(.+)$/==\1 ==/
s/^#(.+)$/==\1 ==/

# bullet points
# -------------
# edoc must see closing </li>
s/^\*(.+)$/<li>\1<\/li>/

# emails, urls
# ------------
s/<([^aA][^@>]+@[^.>]+.[^>]+)>/<a href=\"mailto:\1\">\1<\/a>/
s/<(http[s]?:\/\/[^.>]+.[^>]+)>/<a href=\"\1\">\1<\/a>/

# line breaks
# -----------
s/  $/<br \/>/

# italics, bold
# -------------
s/\*\*([^*]+)\*\*/<b>\1<\/b>/
s/\*([^*]+)\*/<em>\1<\/em>/

# single backticks
# ----------------
# make code quotes
s/`([^`]+)`/<code>\1<\/code>/g

# protect @
# ---------
# leading space or tab indicates use as code sample for, well, edoc
# itself most likely, so escape it. 
s/([ 	\"\'\`]+@)/\1@/g

# Don't work yet, make every other line not parsed.
# headlines by underline === or ---
# ---------------------------------
# demoted to h2 and h3, as h1 is reserved in edoc
# /^[^-=]/{
# N
# s/^(.+)\n=+ *$/== \1 ==/
# s/^(.+)\n-+ *$/=== \1 ===/g
# } 

# ----------------------------------------------------------
# 'powered by Eonblast' http://www.eonblast.com