*"* use this source file for any type declarations (class
*"* definitions, interfaces or data types) you need for method
*"* implementation or private method's signature

types:

  begin of mt_classmap,
    source type seoclsname,
    target type seoclsname,
  end of mt_classmap,

mt_classitem type seoclsname,

  begin of mt_tagitem,
    source type seoclsname,
    tag type string,
  end of mt_tagitem,

  mt_classmap_tt type standard table of mt_classmap,
  mt_classlist_tt type standard table of seoclsname,
  mt_taglist_tt type standard table of mt_tagitem.
