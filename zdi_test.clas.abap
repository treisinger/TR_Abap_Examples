class ZDI_TEST definition
  public
  final
  create public .

*"* public components of class ZDI_TEST
*"* do not include other source files here!!!
public section.
  interface Z_DEPINJ_TEST load .

  interfaces ZDI_IF .
protected section.
*"* protected components of class ZDI_TEST
*"* do not include other source files here!!!

  data TEST type I .
private section.
*"* private components of class ZDI_TEST
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZDI_TEST IMPLEMENTATION.


cdi_inject.
method ZDI_IF~test.
  r = 'test implementation'.
endmethod.                    "ZDI_IF~test
ENDCLASS.
