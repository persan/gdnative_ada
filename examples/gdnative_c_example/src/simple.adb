with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO;
with Ada.Exceptions;
with GNAT.Source_Info;
package body Simple is
   use Ada.Text_IO;
   use GNAT.Source_Info;

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;
  
   use all type GDNATIVE_API_TYPES;

   Core_Api         : Godot_Gdnative_Core_Api_Struct_Ptr;
   Nativescript_Api : Godot_Gdnative_Ext_Nativescript_Api_Struct_Ptr;

   procedure Godot_Gdnative_Init (P_Options : access Godot_Gdnative_Init_Options) is 
      Cursor : GDnative_Api_Struct_Pointers.Pointer;
   begin
      pragma Debug (Put_Line (Source_Location & ":> " & GNAT.Source_Info.Enclosing_Entity));
      Core_Api := P_Options.Api_Struct;
      Cursor := Core_Api.Extensions;
      for I in 1 .. Core_Api.Num_Extensions loop
         case Cursor.all.C_Type is
         when GDNATIVE_EXT_NATIVESCRIPT =>
            Nativescript_Api := To_Api_Struct_Ptr (Cursor.all);
         when others => null;
         end case;
         GDnative_Api_Struct_Pointers.Increment (Cursor);
      end loop;
      pragma Debug (Put_Line (Source_Location & ":< " & GNAT.Source_Info.Enclosing_Entity));
   exception
      when Error : others =>
         declare
            pragma Debug (Put_Line (Source_Location & ":< " & GNAT.Source_Info.Enclosing_Entity & ":" & Ada.Exceptions.Exception_Information (Error)));

            C_Error_Info : ICS.chars_ptr := ICS.New_String (Ada.Exceptions.Exception_Information (Error));
         begin
            P_Options.Report_Loading_Error (P_Options.Gd_Native_Library, C_Error_Info);
            ICS.Free (C_Error_Info);
         end;
   end;

   procedure Godot_Gdnative_Terminate (P_Options : access Godot_Gdnative_Terminate_Options) is begin
      Put_Line (Source_Location & ":> " & GNAT.Source_Info.Enclosing_Entity);
      Core_Api         := null;
      Nativescript_Api := null;
      pragma Debug (Put_Line (Source_Location & ":< " & GNAT.Source_Info.Enclosing_Entity));
   end;
   Phony : aliased String (1 .. 1024);
   Create     : aliased Godot_Instance_Create_Func  := (Object.Simple_Constructor'Access,  Phony'Address  , null);
   Destroy    : aliased Godot_Instance_Destroy_Func := (Object.Simple_Destructor'Access, Phony'Address, null);
   Get_Data   : aliased Godot_Instance_Method       := (Object.Simple_Get_Data'Access, Phony'Address, null);
   Attributes : aliased Godot_Method_Attributes     := (Rpc_Type => GODOT_METHOD_RPC_MODE_DISABLED);
   
   Simple_Ptr    : ICS.chars_ptr := ICS.New_String ("Simple");
   Reference_Ptr : ICS.chars_ptr := ICS.New_String ("Reference");
   Get_Data_Ptr  : ICS.chars_ptr := ICS.New_String ("get_data");

   procedure Godot_Nativescript_Init (P_Handle : Nativescript_Handle) is 


   begin
      pragma Debug (Put_Line (Source_Location & ":> " & GNAT.Source_Info.Enclosing_Entity));
      Nativescript_Api.Godot_Nativescript_Register_Class (P_Handle, Simple_Ptr, Reference_Ptr, Create, Destroy);
      Nativescript_Api.Godot_Nativescript_Register_Method (P_Handle, Simple_Ptr, Get_Data_Ptr, Attributes, Get_Data);
      pragma Debug (Put_Line (Source_Location & ":< " & GNAT.Source_Info.Enclosing_Entity));
   end;

   package body Object is
  
      type User_Data_Struct is record
         Data : aliased Godot_String;
      end record;

      function Simple_Constructor (P_Instance    : System.Address;
                                   P_Method_Data : System.Address)
                                   return System.Address
      is
         P_User_Data : System.Address;
      begin
         pragma Debug (Put_Line (Source_Location & ":> " & GNAT.Source_Info.Enclosing_Entity));
         P_User_Data := Core_Api.Godot_Alloc (User_Data_Struct'Size);
         declare
            User_Data : User_Data_Struct;
            for User_Data'Address use P_User_Data;
            pragma Import (C, User_Data);
            Data : IC.Wchar_Array := IC.To_C ("World from GDNative!");
         begin
            Core_Api.Godot_String_New (User_Data.Data'Access);
            Core_Api.Godot_String_New_With_Wide_String (User_Data.Data'Access, Data (Data'First)'Access, Data'Length);
            
         end;         
         pragma Debug (Put_Line (Source_Location & ":< " & GNAT.Source_Info.Enclosing_Entity));
         return P_User_Data;
      end;

      procedure Simple_Destructor (P_Instance    : System.Address;
                                   P_Method_Data : System.Address;
                                   P_User_Data   : System.Address)
      is 
         User_Data : User_Data_Struct;
         for User_Data'Address use P_User_Data;
         pragma Import (C, User_Data);
      begin
         pragma Debug (Put_Line (Source_Location & ":> " & GNAT.Source_Info.Enclosing_Entity));
         Core_Api.Godot_String_Destroy (User_Data.Data'Access);
         Core_Api.Godot_Free (P_User_Data);
         pragma Debug (Put_Line (Source_Location & ":< " & GNAT.Source_Info.Enclosing_Entity));
      end;

      function Simple_Get_Data (P_Instance    : System.Address;
                                P_Method_Data : System.Address;
                                P_User_Data   : System.Address;
                                P_Num_Args    : IC.int;
                                P_Args        : Godot_Instance_Method_Args_Ptrs.Pointer)
                                return Godot_Variant
      is
         User_Data : User_Data_Struct;
         for User_Data'Address use P_User_Data;
         pragma Import (C, User_Data);
         Variant : aliased Godot_Variant;
      begin
         pragma Debug (Put_Line (Source_Location & ":> " & GNAT.Source_Info.Enclosing_Entity));
         Core_Api.Godot_Variant_New_String (Variant'Access, User_Data.Data'Access);
         pragma Debug (Put_Line (Source_Location & ":< " & GNAT.Source_Info.Enclosing_Entity));
         return Variant;
      end;

   end;
  
end;
