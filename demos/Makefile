out_path = ../target
pascal = ../target/debug/uncle_pascal
units = ../units

tests = test/RcRecord \
	test/RcSimple \
	test/RcNested \
	test/RcArray \
	test/RcWeak \
	HelloWorld \
	Constructor \
	Consts \
#	Enumeration \
	FunctionArgModifier \
	FunctionPointer \
	Generics \
	Interfaces \
	Preprocessor \
	Raise \
	ReadFile \
	VariantParts \
	WithStatement
	
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
	clang-format -i $(out_path)/$@ -style="{ColumnLimit: 0}"

.PHONY: check