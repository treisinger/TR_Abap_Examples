class ZDI_DEFAULT definition
  public
  create public .

*"* public components of class ZDI_DEFAULT
*"* do not include other source files here!!!
public section.

  interfaces ZDI_IF .

  aliases TEST
    for ZDI_IF~TEST .

  methods CONSTRUCTOR
    importing
      value(I_INST) type ref to data optional .
protected section.
*"* protected components of class ZDI_DEFAULT
*"* do not include other source files here!!!
private section.
*"* private components of class ZDI_DEFAULT
*"* do not include other source files here!!!

  data LIF_MOO type ref to ZDI_TEST .
ENDCLASS.



CLASS ZDI_DEFAULT IMPLEMENTATION.


method constructor .

*  if i_inst is initial.
*    zdi=>create_inst(
*      changing c_ref = i_inst ).
*  endif.
*
*  zdi=>create_inst(
*    changing c_ref = lif_moo ).

*  I_INST
endmethod.


method ZDI_IF~test.
  r = 'default implementation'.
endmethod.
ENDCLASS.
