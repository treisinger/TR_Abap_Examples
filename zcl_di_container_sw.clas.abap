class ZCL_DI_CONTAINER_SW definition
  public
  final
  create public .

public section.

*"* public components of class ZCL_DI_CONTAINER_SW
*"* do not include other source files here!!!
  types T_SCOPE type CHAR1 .

  constants SCOPE_TRANSIENT type T_SCOPE value 'T'. "#EC NOTEXT
  constants SCOPE_SINGLETON type T_SCOPE value 'S'. "#EC NOTEXT

  methods BIND
    importing
      !SERVICE type STRING
      !TARGET type STRING optional
      !SCOPE type T_SCOPE default 'T' .
  methods GET
    importing
      !SERVICE type CLIKE
    returning
      value(RESULT) type ref to OBJECT .
protected section.
*"* protected components of class ZCL_DI_CONTAINER_SW
*"* do not include other source files here!!!
private section.

  types:
*"* private components of class ZCL_DI_CONTAINER_SW
*"* do not include other source files here!!!
    BEGIN OF t_binding,
      service       TYPE string,
      service_descr TYPE REF TO cl_abap_objectdescr,
      target_descr  TYPE REF TO cl_abap_classdescr,
      scope         TYPE t_scope,
      singleton     TYPE REF TO object,
    END OF t_binding .
  types:
    t_binding_tab TYPE HASHED TABLE OF t_binding WITH UNIQUE KEY service .

  data BINDINGS type T_BINDING_TAB .

  methods CREATE_BINDING
    importing
      !SERVICE_DESCR type ref to CL_ABAP_OBJECTDESCR
      !TARGET_DESCR type ref to CL_ABAP_CLASSDESCR optional
      !SCOPE type T_SCOPE default 'T'
    returning
      value(RESULT) type ref to T_BINDING .
  methods GET_CONSTRUCTOR_PARMBIND
    importing
      !TARGET_DESCR type ref to CL_ABAP_CLASSDESCR
    returning
      value(RESULT) type ref to ABAP_PARMBIND_TAB .
ENDCLASS.



CLASS ZCL_DI_CONTAINER_SW IMPLEMENTATION.


method BIND.

  data:
    service_descr type ref to cl_abap_objectdescr,
    target_descr  type ref to cl_abap_classdescr.

  service_descr ?= cl_abap_objectdescr=>describe_by_name( service ).

  if not target is initial.
    target_descr ?= cl_abap_classdescr=>describe_by_name( target ).
  endif.

  create_binding(
    service_descr = service_descr
    target_descr = target_descr
    scope = scope ).

endmethod.


method CREATE_BINDING.

  data binding type t_binding.

  if not target_descr is initial.
    binding-target_descr =  target_descr.
  else.
    binding-target_descr ?= service_descr.
  endif.

  assert binding-target_descr->is_instantiatable( ) eq abap_true.

  binding-service       = service_descr->absolute_name.
  binding-service_descr = service_descr.
  binding-scope         = scope.

  insert binding into table bindings reference into result.

endmethod.


method GET.

  data:
    binding       type ref to t_binding,
    parmbindtab   type ref to abap_parmbind_tab,
    service_descr type ref to cl_abap_objectdescr.

  service_descr ?= cl_abap_objectdescr=>describe_by_name( service ).

  read table bindings reference into binding with table key service = service_descr->absolute_name.
  if sy-subrc ne 0.
    binding = create_binding( service_descr ).
  endif.

  if binding->scope eq scope_singleton and binding->singleton is bound.
    result = binding->singleton.
    return.
  endif.

  parmbindtab = get_constructor_parmbind( binding->target_descr ).

  if parmbindtab->* is initial.
    create object result type (binding->target_descr->absolute_name).
  else.
    create object result type (binding->target_descr->absolute_name)
      parameter-table parmbindtab->*.
  endif.

  if binding->scope eq scope_singleton and not binding->singleton is bound.
    binding->singleton = result.
  endif.

endmethod.


method GET_CONSTRUCTOR_PARMBIND.

  data:
    constructor type ref to abap_methdescr,
    parmdescr   type ref to abap_parmdescr,
    refdescr    type ref to cl_abap_refdescr,
    dependency  type ref to cl_abap_objectdescr,
    parmbind    type abap_parmbind.

  field-symbols <fs> type any.

  create data result.

  read table target_descr->methods
      reference into constructor
      with key name = 'CONSTRUCTOR'.

  if sy-subrc eq 0.
    loop at constructor->parameters reference into parmdescr.
      assert parmdescr->type_kind eq cl_abap_objectdescr=>typekind_oref.
      assert parmdescr->parm_kind eq cl_abap_objectdescr=>importing.

      refdescr ?= target_descr->get_method_parameter_type(
          p_method_name = constructor->name
          p_parameter_name = parmdescr->name ).

      create data parmbind-value type handle refdescr.
      assign parmbind-value->* to <fs>.

      dependency ?= refdescr->get_referenced_type( ).
      <fs> ?= get( dependency->absolute_name ).
*      <fs> ?= get( service = dependency->get_relative_name( ) ). "'ZIF_BP_SERVICE'

      parmbind-name = parmdescr->name.
      parmbind-kind = cl_abap_objectdescr=>exporting.
      insert parmbind into table result->*.
    endloop.
  endif.

endmethod.
ENDCLASS.
