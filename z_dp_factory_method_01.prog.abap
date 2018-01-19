*&---------------------------------------------------------------------*
*& Report  Z_DP_FACTORY_METHOD_01
*&
*&---------------------------------------------------------------------*
*& Example for factory method design pattern
*& This is a more an example of simple factory rather than a real
*& factory method.
*&
*&---------------------------------------------------------------------*
report  z_dp_factory_method_01.

*----------------------------------------------------------------------*
*       CLASS lcl_sale_document DEFINITION
*----------------------------------------------------------------------*
* Sale document
*----------------------------------------------------------------------*
class lcl_sale_document definition abstract.
  public section.
    methods: write abstract.
endclass.                    "lcl_sale_document DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_quotation  DEFINITIO
*----------------------------------------------------------------------*
* Quotation
*----------------------------------------------------------------------*
class lcl_quotation definition
      inheriting from lcl_sale_document.
  public section.
    methods: write redefinition.
endclass.                    "lcl_quotation  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS lcl_quotation IMPLEMENTATION
*----------------------------------------------------------------------*
* Quotation implementation
*----------------------------------------------------------------------*
class lcl_quotation implementation.
  method write.
    write: 'Quotation'.
  endmethod.                    "write
endclass.                    "lcl_quotation IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_order  DEFINITIO
*----------------------------------------------------------------------*
* Sale order
*----------------------------------------------------------------------*
class lcl_order definition
      inheriting from lcl_sale_document.
  public section.
    methods: write redefinition.
endclass.                    "lcl_order  DEFINITIO

*----------------------------------------------------------------------*
*       CLASS lcl_order IMPLEMENTATION
*----------------------------------------------------------------------*
* Sale Order implementation
*----------------------------------------------------------------------*
class lcl_order implementation.
  method write.
    write: 'Order'.
  endmethod.                    "write
endclass.                    "lcl_order IMPLEMENTATION

*----------------------------------------------------------------------*
*       INTERFACE lif_sale_document_factory IMPLEMENTATION
*----------------------------------------------------------------------*
* Sale document factor interface
*----------------------------------------------------------------------*
interface lif_sale_document_factory.
  methods:
        create importing im_docty type vbtyp
              returning value(re_doc) type ref to lcl_sale_document.
endinterface.                    "lif_sale_document_factory IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_sale_document_factory DEFINITION
*----------------------------------------------------------------------*
* Sale document factory
*----------------------------------------------------------------------*
class lcl_sale_document_factory definition.
  public section.
    interfaces:
      lif_sale_document_factory.
    aliases: create for lif_sale_document_factory~create.
endclass.                    "lcl_sale_document_factory DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_sale_document_factory IMPLEMENTATION
*----------------------------------------------------------------------*
* Sale document factory implementation
*----------------------------------------------------------------------*
class lcl_sale_document_factory implementation.
  method create.
    data: lo_doc type ref to lcl_sale_document.
    case im_docty.
      when 'B'.
        create object lo_doc type lcl_quotation.
      when 'C'.
        create object lo_doc type lcl_order.
      when others.
        " default, create order
        create object lo_doc type lcl_order.
    endcase.
    re_doc = lo_doc.
  endmethod.                    "create
endclass.                    "lcl_sale_document_factory IMPLEMENTATION

data:
    go_doc type ref to lcl_sale_document,
    go_sale_document_factory type ref to lif_sale_document_factory.

parameters:
    pa_docty type vbtyp. " B quotation, C order

start-of-selection.
  " create factory
  create object go_sale_document_factory
      type lcl_sale_document_factory.

  " create sale document at runtime.
  go_doc = go_sale_document_factory->create( pa_docty ).
  go_doc->write( ).
