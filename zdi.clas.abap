class ZDI definition
  public
  final
  create public .

*"* public components of class ZDI
*"* do not include other source files here!!!
public section.

  class-data MVB_MODE type CHAR4 read-only .
  constants MCC_MODE_PURE type CHAR4 value 'PURE'. "#EC NOTEXT
  constants MCC_MODE_TEST type CHAR4 value 'TEST'. "#EC NOTEXT
  constants MCC_MODE_STANDARD type CHAR4 value 'STND'. "#EC NOTEXT
  constants MCC_TAG_TST type SEOCLSNAME value 'Z_DEPINJ_TEST'. "#EC NOTEXT
  constants MCC_TAG_ALT type SEOCLSNAME value 'Z_DEPINJ_ALTERNATIVE'. "#EC NOTEXT
  constants MCC_TAG_DEF type SEOCLSNAME value 'Z_DEPINJ_DEFAULT'. "#EC NOTEXT
  constants MOO type CHAR100 value ''. "#EC NOTEXT

  class-methods CLASS_CONSTRUCTOR .
  class-methods CREATE_INST
    changing
      !C_REF type ANY
    exceptions
      NONDICTIONARY_TYPE
      NOT_A_REFERENCE
      NO_IMPLEMENTING_CLASSES
      INSTANTIATION_FAILED .
  class-methods TEST_MODE .
  class-methods PURE_MODE .
protected section.
*"* protected components of class ZDI
*"* do not include other source files here!!!
private section.
*"* private components of class ZDI
*"* do not include other source files here!!!

  class-data MI_CACHE type MT_CLASSMAP_TT .

  class-methods EXPLODE_DESCENDENTS
    changing
      !C_CLASSLIST type MT_CLASSLIST_TT .
  class-methods FETCH_CLASS_TAGS
    importing
      !I_CLASSLIST type MT_CLASSLIST_TT
    exporting
      !E_TAGLIST type MT_TAGLIST_TT .
  class-methods GET_IMP_CLASSES
    importing
      !I_INTERFACE type SEOCLSNAME
    exporting
      !E_CLASSES type MT_CLASSLIST_TT .
ENDCLASS.



CLASS ZDI IMPLEMENTATION.


method class_constructor.
  mvb_mode = mcc_mode_standard.
endmethod.


method create_inst.

  data:
    li_ifcl_items     type mt_classlist_tt,
    lvc_ifcl_item     type mt_classitem,
    li_tags           type mt_taglist_tt,
    lr_tag            type mt_tagitem,
    lr_map            type mt_classmap,
    lcl_descr_ref     type ref to cl_abap_refdescr,
    lcl_obj_ref       type ref to cl_abap_objectdescr,
    lcl_refdscr       type ref to cl_abap_refdescr,
    lcl_abap_typdscr  type ref to cl_abap_typedescr.

* determine the dictionary type of the reference that was passed in..
  lcl_descr_ref ?= cl_abap_refdescr=>describe_by_data( c_ref ).
  lcl_abap_typdscr = lcl_descr_ref->get_referenced_type( ).

  lvc_ifcl_item = lcl_abap_typdscr->get_relative_name( ).

  if lvc_ifcl_item is initial. " needs work..
    raise nondictionary_type.
  endif.

* check our cache..
  read table mi_cache into lr_map
    with key source = lvc_ifcl_item.

  if sy-subrc eq 0.
    create object c_ref type (lr_map-target).
    return.
  endif.

  lr_map-source = lvc_ifcl_item.

* input can be a class or an interface whose imp'ing classes we want..
  case lcl_abap_typdscr->type_kind.

    when cl_abap_typedescr=>typekind_class.

      append lvc_ifcl_item to li_ifcl_items.

    when cl_abap_typedescr=>typekind_intf.

      get_imp_classes( " classes that implement the passed in interface
        exporting
          i_interface = lvc_ifcl_item
        importing
          e_classes = li_ifcl_items
      ).

    when others.
      raise not_a_reference.
  endcase.

  explode_descendents( " get entire descendent tree..
    changing c_classlist = li_ifcl_items ).

  if li_ifcl_items is initial.
    raise no_implementing_classes.
  endif.

  fetch_class_tags(
    exporting
      i_classlist = li_ifcl_items
    importing
      e_taglist = li_tags
  ).

  do.
    case mvb_mode.
      when mcc_mode_test.
* if we're in test mode, read first test class if exists..
        get_target_with_tag mcc_tag_tst.
      when mcc_mode_pure.
* if we're in pure mode, read first default class if exists..
        get_target_with_tag mcc_tag_def.
    endcase.

* see if an alternative class exists..
    get_target_with_tag mcc_tag_alt.

* see if we have a class marked as standard..
    get_target_with_tag mcc_tag_def.

* fall through, when no keywords found just return first class..
    read table li_ifcl_items into lvc_ifcl_item
      index 1.
    exit.
  enddo.

  if lvc_ifcl_item is initial.
    raise no_implementing_classes.
  endif.

  try.

* test the constructor of the class to determine whether it
* needs constructor injection..
      if lvc_ifcl_item ne lcl_abap_typdscr->get_relative_name( ).
        lcl_obj_ref ?=
          cl_abap_objectdescr=>describe_by_name( lvc_ifcl_item ).
      else.
        lcl_obj_ref ?= lcl_abap_typdscr.
      endif.

      constants:
        lcc_constructor type abap_methname value 'CONSTRUCTOR'.

      data:
        lr_method type abap_methdescr,
        lr_paramdesc type abap_parmdescr,
        li_bindparams type abap_parmbind_tab,
        lr_bindparam type abap_parmbind.

      field-symbols:
        <fs_param_val> type any.

      read table lcl_obj_ref->methods into lr_method
        with key name = lcc_constructor.

      loop at lr_method-parameters into lr_paramdesc
          where type_kind eq cl_abap_typedescr=>typekind_oref
            and is_optional eq space.

        lr_bindparam-name = lr_paramdesc-name.

        if lr_paramdesc-parm_kind eq cl_abap_objectdescr=>importing.
          lr_bindparam-kind = cl_abap_objectdescr=>exporting.
        else.
          lr_bindparam-kind = cl_abap_objectdescr=>changing.
        endif.

        lcl_obj_ref->get_method_parameter_type(
          exporting
            p_method_name = lcc_constructor
            p_parameter_name = lr_paramdesc-name
          receiving
            p_descr_ref = lcl_abap_typdscr ).

        lcl_refdscr ?= lcl_abap_typdscr.
        lcl_abap_typdscr = lcl_refdscr->get_referenced_type( ).

        create data lr_bindparam-value
          type ref to (lcl_abap_typdscr->absolute_name).

        assign lr_bindparam-value->* to <fs_param_val>.
        clear <fs_param_val>.

        create_inst( changing c_ref = <fs_param_val> ).

        if <fs_param_val> is not initial.
          insert lr_bindparam into table li_bindparams.
        endif.
      endloop.

      if li_bindparams is initial.
        create object c_ref type (lvc_ifcl_item).
      else.
        create object c_ref type (lvc_ifcl_item)
          parameter-table li_bindparams.
      endif.

    catch cx_sy_create_object_error.
      raise instantiation_failed.
  endtry.

  lr_map-target = lvc_ifcl_item.
  append lr_map to mi_cache.

endmethod.


method explode_descendents.

  data:
    li_descendents type mt_classlist_tt.

* first, build up list of all descendents of classes in list..
  li_descendents = c_classlist.
  do.
    select clsname from vseoextend
      into table li_descendents
      for all entries in li_descendents
      where refclsname eq li_descendents-table_line.

    if sy-subrc ne 0. " exit loop when done..
      exit.
    endif.

    append lines of li_descendents to c_classlist.
  enddo.

endmethod.


method fetch_class_tags.

  type-pools: seok.

  data:
    iseotypepls       type standard table of seotypepls,
    lr_seotypepls     type seotypepls,
    lr_tag            type mt_tagitem.

  select clsname typegroup into table iseotypepls
    from seotypepls
    for all entries in i_classlist
    where clsname eq i_classlist-table_line
      and tputype eq seot_tputype_interfacedeferred.

  loop at iseotypepls into lr_seotypepls.
    lr_tag-source = lr_seotypepls-clsname.
    lr_tag-tag = lr_seotypepls-typegroup.
    append lr_tag to e_taglist.
  endloop.

endmethod.


method get_imp_classes.

  type-pools: seok.

  select clsname from vseoimplem
    into table e_classes
    where refclsname eq i_interface
      and version ne seoc_version_deleted.

  delete adjacent duplicates from e_classes.

endmethod.


method pure_mode.

  mvb_mode = mcc_mode_pure.
  refresh mi_cache.

endmethod.


method test_mode.

  mvb_mode = mcc_mode_test.
  refresh mi_cache.

endmethod.
ENDCLASS.
