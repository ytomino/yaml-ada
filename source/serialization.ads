with Ada.Containers;
with Ada.Strings.Unbounded;
package Serialization is
	pragma Preelaborate;
	
	type Stream_Element_Kind is (
		Value,
		Enter_Mapping,
		Leave_Mapping,
		Enter_Sequence,
		Leave_Sequence,
		End_Of_Stream);
	
	type Direction is (Reading, Writing);
	
	type Serializer (Direction : Serialization.Direction) is limited private;
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Ada.Strings.Unbounded.Unbounded_String);
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Ada.Strings.Unbounded.Unbounded_String;
		Default : in Ada.Strings.Unbounded.Unbounded_String);
	procedure IO (
		Object : not null access Serializer;
		Value : in out Ada.Strings.Unbounded.Unbounded_String);
	procedure IO (
		Object : not null access Serializer;
		Value : in out Ada.Strings.Unbounded.Unbounded_String;
		Default : in Ada.Strings.Unbounded.Unbounded_String);
	
	generic
		type Custom_Type is private;
		with function Image (S : Custom_Type) return String;
		with function Value (S : String) return Custom_Type;
		Triming : in Boolean := False;
	package IO_Custom is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Custom_Type); 
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Custom_Type;
			Default : in Custom_Type);
	end IO_Custom;
	
	generic
		type Enumeration_Type is (<>);
	package IO_Enumeration is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Enumeration_Type); 
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Enumeration_Type;
			Default : in Enumeration_Type);
	private
		package Enum_IO is new IO_Custom (
			Enumeration_Type,
			Enumeration_Type'Image,
			Enumeration_Type'Value,
			Triming => True);
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Enumeration_Type) renames Enum_IO.IO;
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Enumeration_Type;
			Default : in Enumeration_Type) renames Enum_IO.IO;
	end IO_Enumeration;
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Integer);
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Integer;
		Default : in Integer);
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Boolean);
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Value : in out Boolean;
		Default : in Boolean);
	
	-- mapping
	
	procedure IO (
		Object : not null access Serializer;
		Name : in String;
		Callback : not null access procedure);
	procedure IO (
		Object : not null access Serializer;
		Callback : not null access procedure);
	
	-- sequence
	
	generic
		type Container_Type is limited private;
		type Cursor is private;
		type Element_Type is private;
		Default : in Element_Type;
		with function Last (Container : Container_Type) return Cursor is <>;
		with procedure Iterate (
			Container : in Container_Type;
			Process : not null access procedure (Position : Cursor)) is <>;
		with procedure Update_Element (
			Container : in out Container_Type;
			Position : in Cursor;
			Process : not null access procedure (Item : in out Element_Type)) is <>;
		with procedure Append (
			Container : in out Container_Type;
			New_Item : in Element_Type;
			Count : in Ada.Containers.Count_Type := 1) is <>;
		with procedure Clear (Container : in out Container_Type) is <>;
	package IO_List is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Container_Type;
			Callback : not null access procedure (Item : in out Element_Type));
		procedure IO (
			Object : not null access Serializer;
			Value : in out Container_Type;
			Callback : not null access procedure (Item : in out Element_Type));
	end IO_List;
	
	generic
		type Index_Type is (<>);
		type Element_Type is private;
		type Array_Type is array (Index_Type range <>) of Element_Type;
		type Array_Access is access Array_Type;
		Default : in Element_Type;
		with procedure Free (X : in out Array_Access) is <>;
	package IO_Array is
		procedure IO (
			Object : not null access Serializer;
			Name : in String;
			Value : in out Array_Access;
			Callback : not null access procedure (Item : in out Element_Type));
		procedure IO (
			Object : not null access Serializer;
			Value : in out Array_Access;
			Callback : not null access procedure (Item : in out Element_Type));
	end IO_Array;
	
private
	
	type State is (In_Mapping, In_Sequence);
	pragma Discard_Names (State);
	
	type Reader is abstract tagged limited null record;
	
	function Next_Kind (Object : not null access Reader)
		return Stream_Element_Kind is abstract;
	function Next_Name (Object : not null access Reader)
		return not null access constant String is abstract;
	function Next_Value (Object : not null access Reader)
		return not null access constant String is abstract;
	procedure Advance (
		Object : not null access Reader;
		Position : in State) is abstract;
	procedure Advance_Structure (
		Object : not null access Reader;
		Position : in State);
	
	type Writer is abstract tagged limited null record;
	
	procedure Put (
		Object : not null access Writer;
		Name : in String;
		Item : in String) is abstract;
	procedure Enter_Mapping (
		Object : not null access Writer;
		Name : in String) is abstract;
	procedure Leave_Mapping (Object : not null access Writer) is abstract;
	procedure Enter_Sequence (
		Object : not null access Writer;
		Name : in String) is abstract;
	procedure Leave_Sequence (Object : not null access Writer) is abstract;
	
	type Serializer (Direction : Serialization.Direction) is limited record
		case Direction is
			when Reading =>
				Reader : not null access Serialization.Reader'Class;
			when Writing =>
				Writer : not null access Serialization.Writer'Class;
		end case;
	end record;
	
end Serialization;
