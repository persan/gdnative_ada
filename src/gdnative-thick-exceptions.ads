
with Ada.Exceptions;

package GDNative.Thick.Exceptions is
  
  procedure Put_Warning (Message    : in Wide_String);
  procedure Put_Error   (Occurrence : in Ada.Exceptions.Exception_Occurrence);

end;