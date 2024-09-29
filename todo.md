* can we get rid of different funciton call types for interfaces/functions? maybe just place that iwth methods that panic if hte expr target is the wrong kind?
* properly initialize function local vars (IR)
* type cache so we don't need to construct/pass around type objects all the time in the typechecker (This is really slow)
* stop outputting empty releaser funcs where there's nothing ot be released
* parser: newlines/escape chars in string literals~~~~

# critical

* init code should be stored in separate functions with their own stack trace entries
* allow trailing comma in collection init!!
* c codegen: escaping! can't do ""
* better message when iface impl doens't match decl sig
* unique compiler error codes
* does symbol with same name declared in branch scope confuse the initialization consolidation?

# big features

* namespaced methods inside classes/records <------- GET RID OF METHOD OF IFACE SYNTAX
* enums/sets/range types
* generic interfaces/generic methods
* @-attributes - replace `external` etc with these (eventually make them available at runtime but not now)
* proper function overloads (one declared name, multiple signatures)

# todos

* undefined symbol checking can probably be moved to IR codegen rather than during typechecking 
