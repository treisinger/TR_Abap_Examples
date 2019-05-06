class ZCL_BC_INJECTOR definition
  public
  create public .

public section.

  class-methods CREATE_VIA_INJECTION
    changing
      !CO_OBJECT type ANY .
  class-methods DURING_CONSTRUCTION
    importing
      !FOR_PARAMETER type STRING
      !USE_VALUE type ANY .
  class-methods INSTEAD_OF
    importing
      !USING_MAIN_CLASS type SEOCLASS-CLSNAME
      !USE_SUB_CLASS type SEOCLASS-CLSNAME .
protected section.
private section.

  types:
    BEGIN OF m_typ_created_objects,
           clsname TYPE seoclass-clsname,
           object  TYPE REF TO object,
    END OF m_typ_created_objects .
  types:
    BEGIN OF m_typ_parameter_values,
           identifier TYPE abap_parmname,                  "in case more than one of same data element
           rollname   TYPE rollname,
           do_value   TYPE REF TO data,
    END OF m_typ_parameter_values .
  types:
    BEGIN OF m_typ_specific_classes,
           main_class TYPE seoclass-clsname,
           sub_class  TYPE seoclass-clsname,
    END OF   m_typ_specific_classes .

  class-data:
    mt_created_objects TYPE HASHED TABLE OF m_typ_created_objects
               WITH UNIQUE KEY clsname .
  class-data:
    mt_parameter_values TYPE HASHED TABLE OF m_typ_parameter_values
               WITH UNIQUE KEY identifier rollname .
  class-data:
    mt_sub_classes_to_use TYPE STANDARD TABLE OF m_typ_specific_classes .
  type-pools ABAP .
  constants MC_CONSTRUCTOR type ABAP_METHNAME value 'CONSTRUCTOR'. "#EC NOTEXT

  class-methods DATA_ELEMENT_PARAMETER
    importing
      !IO_PARAMETER_DESCRIBE_BY_TYPE type ref to CL_ABAP_TYPEDESCR
      !IS_SIGNATURE_VALUES type ABAP_PARMBIND
    changing
      !CT_SIGNATURE_VALUES type ABAP_PARMBIND_TAB .
  class-methods OBJECT_PARAMETER
    importing
      !IO_PARAMETER_DESCRIBE_BY_TYPE type ref to CL_ABAP_TYPEDESCR
      !IS_SIGNATURE_VALUES type ABAP_PARMBIND
    changing
      !CT_SIGNATURE_VALUES type ABAP_PARMBIND_TAB .
  class-methods CREATE_PARAMETER_OBJECT
    importing
      !ID_CLASS_TYPE_TO_CREATE type SEOCLASS-CLSNAME
      !IT_SIGNATURE_VALUES type ABAP_PARMBIND_TAB
    changing
      !CO_OBJECT type ANY .
  class-methods DETERMINE_CLASS_TO_CREATE
    importing
      !ID_CLASS_PASSED_IN type SEOCLASS-CLSNAME
      !IO_CLASS_IN_TYPE_DETAILS type ref to CL_ABAP_TYPEDESCR
    exporting
      !ED_CLASS_TYPE_TO_CREATE type SEOCLASS-CLSNAME
      !EO_CLASS_TO_CREATE_TYPE_DETAIL type ref to CL_ABAP_TYPEDESCR .
  class-methods FILL_CONSTRUCTOR_PARAMETERS
    importing
      !IO_CLASS_TO_CREATE_TYPE_DETAIL type ref to CL_ABAP_TYPEDESCR
    exporting
      !ET_SIGNATURE_VALUES type ABAP_PARMBIND_TAB .
ENDCLASS.



CLASS ZCL_BC_INJECTOR IMPLEMENTATION.


METHOD create_parameter_object.
* Local Variables
  DATA: ls_created_objects LIKE LINE OF mt_created_objects.

  TRY.
      IF it_signature_values IS INITIAL.
        CREATE OBJECT co_object TYPE (id_class_type_to_create).
      ELSE.
        CREATE OBJECT co_object TYPE (id_class_type_to_create)
          PARAMETER-TABLE it_signature_values.
      ENDIF.

      "Hooray, we have created an object instance
      "Let us store this in a buffer in case we need it again
      "in a minute
      ls_created_objects-clsname = id_class_type_to_create.
      ls_created_objects-object  = co_object.              "Up Cast Top Ranking
      INSERT ls_created_objects INTO TABLE mt_created_objects.

    CATCH cx_sy_create_object_error.
      RAISE EXCEPTION TYPE cx_fatal_exception.
  ENDTRY.

ENDMETHOD.


METHOD create_via_injection.
* Local Variables
  DATA: lo_class_in_reference_details  TYPE REF TO cl_abap_refdescr,
        lo_class_in_type_details       TYPE REF TO cl_abap_typedescr,
        lo_class_to_create_type_detail TYPE REF TO cl_abap_typedescr,
        ld_class_passed_in             TYPE seoclass-clsname,
        ld_class_type_to_create        TYPE seoclass-clsname,
        ls_created_objects             LIKE LINE OF mt_created_objects,
        lt_signature_values            TYPE abap_parmbind_tab.

* Determine the class type of the reference object that was passed in
  lo_class_in_reference_details ?= cl_abap_refdescr=>describe_by_data( co_object ).
  lo_class_in_type_details       = lo_class_in_reference_details->get_referenced_type( ).
  ld_class_passed_in             = lo_class_in_type_details->get_relative_name( ).

  "See if we need to create the real class, or a subclass
  determine_class_to_create(
    EXPORTING
      id_class_passed_in             = ld_class_passed_in
      io_class_in_type_details       = lo_class_in_type_details
    IMPORTING
      ed_class_type_to_create        = ld_class_type_to_create
      eo_class_to_create_type_detail = lo_class_to_create_type_detail ).

  READ TABLE mt_created_objects INTO ls_created_objects WITH TABLE KEY clsname = ld_class_type_to_create.

  IF sy-subrc = 0.
    "We already have an instance of this class we can use
    co_object ?= ls_created_objects-object.
    RETURN.
  ENDIF.

  "See if the object we want to create has parameters, and if so, fill them up
  fill_constructor_parameters( EXPORTING io_class_to_create_type_detail = lo_class_to_create_type_detail " Class to Create Type Details
                               IMPORTING et_signature_values            = lt_signature_values ).         " Constructor Parameters

  create_parameter_object( EXPORTING id_class_type_to_create = ld_class_type_to_create   " Exact Class to Create
                                     it_signature_values     = lt_signature_values       " Parameter Values
                           CHANGING  co_object               = co_object ).              " Created Object

ENDMETHOD.


METHOD data_element_parameter.
* Local Variables
  DATA :ld_data_element_name TYPE rollname,
        ls_signature_values  LIKE LINE OF ct_signature_values,
        ls_parameter_values  LIKE LINE OF mt_parameter_values.

  "Get Data Element (Rollname)
  ld_data_element_name = io_parameter_describe_by_type->get_relative_name( ).

  READ TABLE mt_parameter_values INTO ls_parameter_values
  WITH TABLE KEY identifier = is_signature_values-name
                 rollname   = ld_data_element_name.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_fatal_exception.
  ENDIF.

  ls_signature_values = is_signature_values.

  ls_signature_values-value = ls_parameter_values-do_value.

  INSERT ls_signature_values INTO TABLE ct_signature_values.

ENDMETHOD.


METHOD determine_class_to_create.
* Local Variables
  DATA :  ls_sub_classes_to_use LIKE LINE OF mt_sub_classes_to_use.

  CLEAR : ed_class_type_to_create,
          eo_class_to_create_type_detail.

  CASE io_class_in_type_details->kind.
    WHEN cl_abap_typedescr=>kind_class OR
         cl_abap_typedescr=>kind_intf.
* First see if we have an instruction to create a sub-class instead of the main class
* for example a test double
      READ TABLE mt_sub_classes_to_use INTO ls_sub_classes_to_use
      WITH KEY main_class = id_class_passed_in.

      IF sy-subrc = 0.
        "We are creating a sub-class, not the main class
        ed_class_type_to_create        = ls_sub_classes_to_use-sub_class.
        eo_class_to_create_type_detail = cl_abap_typedescr=>describe_by_name( ed_class_type_to_create ).
      ELSEIF io_class_in_type_details->kind = cl_abap_typedescr=>kind_intf.
        "An interface was passed in, we don't know what actual class to create
        RAISE EXCEPTION TYPE cx_fatal_exception.
      ELSE.
        "We are creating an instance of the actual class passed in
        ed_class_type_to_create        = id_class_passed_in.
        eo_class_to_create_type_detail = io_class_in_type_details.
      ENDIF.

    WHEN OTHERS.
      RAISE EXCEPTION TYPE cx_fatal_exception.
  ENDCASE.

ENDMETHOD.


METHOD during_construction.
* Local Variables
  DATA: lo_description       TYPE REF TO cl_abap_typedescr,
        ld_dummy             TYPE string ##needed,
        ld_data_element_name TYPE string,
        ls_parameter_values  LIKE LINE OF mt_parameter_values.

  ls_parameter_values-identifier = for_parameter.

  CREATE DATA ls_parameter_values-do_value LIKE use_value.
  GET REFERENCE OF use_value INTO ls_parameter_values-do_value.

  CHECK sy-subrc = 0.

  CALL METHOD cl_abap_structdescr=>describe_by_data_ref
    EXPORTING
      p_data_ref           = ls_parameter_values-do_value
    RECEIVING
      p_descr_ref          = lo_description
    EXCEPTIONS
      reference_is_initial = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SPLIT lo_description->absolute_name AT '=' INTO ld_dummy ld_data_element_name.

  ls_parameter_values-rollname = ld_data_element_name.

  INSERT ls_parameter_values INTO TABLE mt_parameter_values.

ENDMETHOD.


METHOD fill_constructor_parameters.
* Local Variables
  DATA: lo_class_tc_object_details    TYPE REF TO cl_abap_objectdescr,
        lo_parameter_describe_by_type TYPE REF TO cl_abap_typedescr,
        ls_constructor_method         TYPE abap_methdescr,
        ls_constructor_signature      TYPE abap_parmdescr,
        ls_signature_values           TYPE abap_parmbind.

  REFRESH et_signature_values.

  "To find a the method details of a class defintion we neend to
  "change the analysis instance from TYPE to OBJECT
  lo_class_tc_object_details ?= io_class_to_create_type_detail.

  "All right, let's find out about the constructor of the class we want to
  "create
  READ TABLE lo_class_tc_object_details->methods INTO ls_constructor_method
    WITH KEY name = mc_constructor.

  "We now know all the constructor parameters, let's loop through them
  LOOP AT ls_constructor_method-parameters INTO ls_constructor_signature.

    ls_signature_values-name = ls_constructor_signature-name.

    IF ls_constructor_signature-parm_kind EQ cl_abap_objectdescr=>importing.
      ls_signature_values-kind = cl_abap_objectdescr=>exporting.
    ELSE.
      ls_signature_values-kind = cl_abap_objectdescr=>changing.
    ENDIF.

    lo_parameter_describe_by_type = lo_class_tc_object_details->get_method_parameter_type(
      p_method_name    = mc_constructor
      p_parameter_name = ls_constructor_signature-name ).

    IF ls_constructor_signature-type_kind = cl_abap_typedescr=>typekind_oref.

      object_parameter( EXPORTING io_parameter_describe_by_type = lo_parameter_describe_by_type           " Parameter Object
                                  is_signature_values           = ls_signature_values                     " Parameters
                        CHANGING  ct_signature_values           = et_signature_values ).                  " Parameter Table

    ELSEIF lo_parameter_describe_by_type->kind = cl_abap_typedescr=>kind_elem.

      data_element_parameter( EXPORTING io_parameter_describe_by_type = lo_parameter_describe_by_type           " Parameter Object
                                        is_signature_values           = ls_signature_values                     " Parameters
                              CHANGING  ct_signature_values           = et_signature_values ).                  " Parameter Table

    ENDIF.

  ENDLOOP."Constructor Parameters

ENDMETHOD.


METHOD instead_of.
* Local Variables
  DATA: ls_sub_classes_to_use LIKE LINE OF mt_sub_classes_to_use.

  ls_sub_classes_to_use-main_class = using_main_class.
  ls_sub_classes_to_use-sub_class  = use_sub_class.

  "Add entry at the start, so it takes priority over previous
  "similar entries
  INSERT ls_sub_classes_to_use INTO mt_sub_classes_to_use INDEX 1.

ENDMETHOD.


METHOD object_parameter.
* Local Variables
  DATA :ls_signature_values           LIKE LINE OF ct_signature_values,
        lo_parameter_describe_by_ref  TYPE REF TO cl_abap_refdescr,
        lo_parameter_describe_by_type TYPE REF TO cl_abap_typedescr.

  FIELD-SYMBOLS: <parameter_value> TYPE any.

  ls_signature_values = is_signature_values.

  lo_parameter_describe_by_ref ?= io_parameter_describe_by_type.
  lo_parameter_describe_by_type = lo_parameter_describe_by_ref->get_referenced_type( ).

  CREATE DATA ls_signature_values-value
    TYPE REF TO (lo_parameter_describe_by_type->absolute_name).

  ASSIGN ls_signature_values-value->* TO <parameter_value>.
  CLEAR <parameter_value>.

  create_via_injection( CHANGING co_object = <parameter_value> ).

  CHECK <parameter_value> IS ASSIGNED.
  CHECK <parameter_value> IS NOT INITIAL.

  INSERT ls_signature_values INTO TABLE ct_signature_values.

ENDMETHOD.
ENDCLASS.
