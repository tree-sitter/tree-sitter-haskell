(varid) @variable
(varsym) @operator
(exp_name (conid) @constructor)
(consym) @operator
(modid (conid) @module_name)
(tyconid) @type
(tyconid (conid)) @class
(constr_id) @constructor
(pragma) @pragma
(comment) @comment
(decl_sig name: (varid) @fun_type_name)
(funvar name: (varid) @fun_name)
(constraint class: (class_name (tyconid)) @class)
(decl_class (class_head class: (class_name (tyconid)) @class))
(decl_instance (instance_head class: (class_name (tyconid)) @class))
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
