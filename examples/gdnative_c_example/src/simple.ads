with System;
with Interfaces.C;
with GDNative.Thin; use GDNative.Thin;

package Simple is
   
   procedure Godot_Gdnative_Init (P_Options : access Godot_Gdnative_Init_Options)
     with Export => True, 
     Convention => C,
     External_Name => $Gdnative_Init;
  
   procedure Godot_Gdnative_Terminate (P_Options : access Godot_Gdnative_Terminate_Options)
     with Export => True,
     Convention => C, 
     External_Name => $Gdnative_Terminate;

   procedure Godot_Nativescript_Init (P_Handle : Nativescript_Handle)
     with Export => True,
     Convention => C, 
     External_Name => $Nativescript_Init;

   -------
private
   -------

   package Object is
  
      function Simple_Constructor (P_Instance    : System.Address;
                                   P_Method_Data : System.Address)
                                   return System.Address;
      pragma Convention (C, Simple_Constructor);

      procedure Simple_Destructor (P_Instance    : System.Address;
                                   P_Method_Data : System.Address;
                                   P_User_Data   : System.Address);
      pragma Convention (C, Simple_Destructor);
    
      function Simple_Get_Data (P_Instance    : System.Address;
                                P_Method_Data : System.Address;
                                P_User_Data   : System.Address;
                                P_Num_Args    : Interfaces.C.int;
                                P_Args        : Godot_Instance_Method_Args_Ptrs.Pointer) -- godot_variant **
                                return Godot_Variant;
      pragma Convention (C, Simple_Get_Data);

   end;

end;
