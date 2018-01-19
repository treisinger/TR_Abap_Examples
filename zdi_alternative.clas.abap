class ZDI_ALTERNATIVE definition
  public
  inheriting from ZDI_DEFAULT
  final
  create public .

*"* public components of class ZDI_ALTERNATIVE
*"* do not include other source files here!!!
public section.

  methods ZDI_IF~TEST
    redefinition .
protected section.
*"* protected components of class ZDI_ALTERNATIVE
*"* do not include other source files here!!!
private section.
*"* private components of class ZDI_ALTERNATIVE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZDI_ALTERNATIVE IMPLEMENTATION.


method ZDI_IF~test.
  r = 'alternative implementation'.
endmethod.
ENDCLASS.
