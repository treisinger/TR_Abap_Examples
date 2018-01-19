report  zselscreen_example.
*&---------------------------------------------------------------------*
*& Purpose - Dynamic Selection Screen using the Tabbed Selected Screen
*& Author  - Naimesh Patel
*& URL     -
*&---------------------------------------------------------------------*
*
class lcl_main definition.
  public section.
    types:
      begin of ty_vrm_values,
        key  type char40,
        text type char80,
      end   of ty_vrm_values.
    types: tt_vrm_values type standard table of ty_vrm_values
             with default key.
    types:
      begin of ty_config,
        key  type char10,
        desc type char50,
        dynnr type sy-dynnr,
      end   of ty_config.
    data: t_config type standard table of ty_config.
    methods:
      get_vrm_values
        returning value(rt_values) type tt_vrm_values,
      get_dynnr
        importing iv_objtype type char10
        returning value(rv_dynnr) type sy-dynnr.
endclass.                    "lcl_main DEFINITION
*
data: t_objtypes type lcl_main=>tt_vrm_values.
data: o_main type ref to lcl_main.
*
data: v_vbeln type vbak-vbeln,
      v_erdat type vbak-erdat,
      v_vkorg type vbak-vkorg,
      v_vtweg type vbak-vtweg,
      v_spart type vbak-spart.
* Object type Selector
parameter: p_objtyp type char10 as listbox
             visible length 30 user-command v_obj .
*
* TABBED Selection screen for displaying the screen
selection-screen: begin of tabbed block mytab for 7 lines,
                  tab (20) seltab user-command push1,
                  end of block mytab.
*
* Default screen
selection-screen begin of screen 100 as subscreen.
selection-screen end of screen 100.
*
* for SO
selection-screen begin of screen 200 as subscreen.
select-options: so_vbeln for v_vbeln,
                so_erdat for v_erdat,
                so_vkorg for v_vkorg,
                so_vtweg for v_vtweg,
                so_spart for v_spart.
selection-screen end of screen 200.
*
* for Billing
selection-screen begin of screen 300 as subscreen.
select-options: bi_vbeln for v_vbeln,
                bi_erdat for v_erdat,
                bi_vkorg for v_vkorg,
                bi_vtweg for v_vtweg,
                bi_spart for v_spart.
selection-screen end of screen 300.
*
initialization.
  create object o_main.
  t_objtypes = o_main->get_vrm_values( ).
  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_OBJTYP'
      values = t_objtypes.
*
  mytab-dynnr = o_main->get_dynnr( p_objtyp ).
  mytab-prog = sy-repid.
  mytab-activetab = 'PUSH1'.
*
  seltab = 'Selection Criteria'.
*
* dynamic text for objec type
  %_p_objtyp_%_app_%-text = 'Object Type'.
*
*
at selection-screen output.
* Set the selection screen based on the object type
  mytab-dynnr = o_main->get_dynnr( p_objtyp ).


start-of-selection.
*
*
class lcl_main implementation.
  method get_vrm_values.
    data: ls_config like line of me->t_config.
* ---- Config entries, in real time it comes from table
*
    ls_config-key   = ".             " Default
    ls_config-desc  = ".
    ls_config-dynnr = '0100'.
    append ls_config to me->t_config.
*
    ls_config-key   = 'ORDER'.
    ls_config-desc  = 'Sales Order'.
    ls_config-dynnr = '0200'.
    append ls_config to me->t_config.
*
    ls_config-key   = 'BILLING'.
    ls_config-desc  = 'Billing Document'.
    ls_config-dynnr = '0300'.
    append ls_config to me->t_config.
*
*   vrm set values
    data: ls_vrm like line of rt_values.
    loop at me->t_config into ls_config.
      ls_vrm-key = ls_config-key.
      ls_vrm-text = ls_config-desc.
      append ls_vrm to rt_values.
    endloop.
  endmethod.                    "get_vrm_values
  method get_dynnr.
*   get the screen number
    data: ls_config like line of me->t_config.
    read table me->t_config into ls_config
      with key key = iv_objtype.
    if sy-subrc eq 0.
      rv_dynnr = ls_config-dynnr.
    else.
*     fallback - if default is not maintained in config
      rv_dynnr = '0100'.
    endif.
  endmethod.                    "get_dynnr
endclass.                    "lcl_main IMPLEMENTATION
