# Copyright 2009 Jason Wagner
# Released under the zlib license.  See LICENSE.txt

appname = erlymock

# directories
src := src
ebin := ebin
doc := doc
edoc := doc/edoc
test := test
includedir := include

#erl config
erl_node := -sname e1
erl_path := 
erl_boot := -boot start_sasl
erl_config := 
erlc_flags := +debug_info
#inputs 
sources = $(wildcard ${src}/*.erl)
test_src = $(wildcard ${test}/*_tests.erl)
includes := -I ${includedir}

#outputs
modules := $(basename $(notdir ${sources}))
beams := $(addprefix ${ebin}/,$(addsuffix .beam,${modules}))
products := ${beams} ${app_file} ${rel_file}

################################################################
# Rules
################################################################
.PHONY: all compile shell run doc test testcompile
all: compile

compile: ${sources}
	mkdir -p ${ebin}
	erlc ${erlc_flags} -o ${ebin} ${includes} ${sources}

shell: compile testcompile
	erl ${erl_node} ${erl_path} ${erl_boot} ${erl_config}

testcompile: compile
	erlc ${erlc_flags} -o ${ebin} ${includes} ${test_src}	
	
test: testcompile
	erl ${erl_node} ${erl_path} ${erl_boot} ${erl_config} -noshell -s eunit test $(modules) -s init stop 

doc:
	-mkdir -p doc
	erl -noshell -run edoc_run application "'$(appname)'"

clean:
	rm -f ${products} ebin/*.beam

