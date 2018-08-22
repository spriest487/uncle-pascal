out_path = ../target
pascal = ../target/debug/uncle_pascal
units = ../units

tests = test/RcRecord \
	test/RcSimple \
	test/RcNested \
	test/RcArray
	
define \n


endef

all: build check

build: $(tests:=.exe)
cpp: $(tests:=.cpp)

check: build
	$(foreach testname, $(tests), $(out_path)/$(testname).exe${\n})

%.exe: %.pas
	$(pascal) $< -I $(units) -o $(out_path)/$@

%.cpp: %.pas
	$(pascal) $< -I $(units) -o $(out_path)/$@
	clang-format -i $(out_path)/$@ --style=Mozilla

.PHONY: check