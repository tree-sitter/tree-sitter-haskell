(variable) @variable
(operator) @operator
(exp_name (constructor) @constructor)
(consym) @operator
(modid (constructor) @module_name)
(type) @type
(type) @class
(constr_id) @constructor
(pragma) @pragma
(comment) @comment
(decl_sig name: (variable) @fun_type_name)
(funvar name: (variable) @fun_name)
(constraint class: (class_name (type)) @class)
(class (class_head class: (class_name (type)) @class))
(instance (instance_head class: (class_name (type)) @class))
(integer) @literal
(exp_literal (float)) @literal
(char) @literal
(con_unit) @literal
(con_list) @literal
(tycon_arrow) @operator
(where) @keyword
"module" @keyword
"let" @keyword
"in" @keyword
"class" @keyword
"instance" @keyword
"data" @keyword
"newtype" @keyword
"family" @keyword
"type" @keyword
"import" @keyword
"qualified" @keyword
"as" @keyword
"deriving" @keyword
"via" @keyword
"stock" @keyword
"anyclass" @keyword
"do" @keyword
"mdo" @keyword
"rec" @keyword
"(" @paren
")" @paren
