*"* use this source file for any macro definitions you need
*"* in the implementation part of the class


define get_target_with_tag.
*> &1 tagname
  read table li_tags into lr_tag
    with key tag = &1.
  if sy-subrc eq 0.
    lvc_ifcl_item = lr_tag-source.
    exit.
  endif.
end-of-definition.
